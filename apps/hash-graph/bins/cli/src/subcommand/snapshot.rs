use authorization::{
    backend::{SpiceDbOpenApi, ZanzibarBackend},
    zanzibar::ZanzibarClient,
    AuthorizationApi, NoAuthorization,
};
use clap::Parser;
use error_stack::{Result, ResultExt};
use graph::{
    snapshot::{SnapshotEntry, SnapshotStore},
    store::{
        ontology::GetEntityTypesParams, query::Filter, DatabaseConnectionInfo, DatabasePoolConfig,
        EntityTypeStore, PostgresStorePool, StorePool,
    },
    subgraph::identifier::EntityTypeVertexId,
};
use graph_types::account::AccountId;
use serde_json::json;
use tokio::io;
use tokio_postgres::NoTls;
use tokio_util::codec::{FramedRead, FramedWrite};
use uuid::Uuid;

use crate::error::GraphError;

#[derive(Debug, Parser)]
pub struct SnapshotDumpArgs;

#[derive(Debug, Parser)]
pub struct SnapshotRestoreArgs {
    /// Whether to skip the validation checks.
    #[clap(long)]
    pub skip_validation: bool,
}

#[derive(Debug, Parser)]
pub enum SnapshotCommand {
    Dump(SnapshotDumpArgs),
    Restore(SnapshotRestoreArgs),
    Reinsert,
}

#[derive(Debug, Parser)]
#[clap(version, author, about, long_about = None)]
pub struct SnapshotArgs {
    #[command(subcommand)]
    pub command: SnapshotCommand,

    #[clap(flatten)]
    pub db_info: DatabaseConnectionInfo,

    #[clap(flatten)]
    pub pool_config: DatabasePoolConfig,

    /// The host the Spice DB server is listening at.
    #[clap(long, env = "HASH_SPICEDB_HOST")]
    pub spicedb_host: String,

    /// The port the Spice DB server is listening at.
    #[clap(long, env = "HASH_SPICEDB_HTTP_PORT")]
    pub spicedb_http_port: u16,

    /// The secret key used to authenticate with the Spice DB server.
    #[clap(long, env = "HASH_SPICEDB_GRPC_PRESHARED_KEY")]
    pub spicedb_grpc_preshared_key: Option<String>,
}

pub async fn snapshot(args: SnapshotArgs) -> Result<(), GraphError> {
    SnapshotEntry::install_error_stack_hook();

    let pool = PostgresStorePool::new(&args.db_info, &args.pool_config, NoTls)
        .await
        .change_context(GraphError)
        .map_err(|report| {
            tracing::error!(error = ?report, "Failed to connect to database");
            report
        })?;

    let mut spicedb_client = SpiceDbOpenApi::new(
        format!("{}:{}", args.spicedb_host, args.spicedb_http_port),
        args.spicedb_grpc_preshared_key.as_deref(),
    )
    .change_context(GraphError)?;
    spicedb_client
        .import_schema(include_str!(
            "../../../../../../libs/@local/hash-authorization/schemas/v1__initial_schema.zed"
        ))
        .await
        .change_context(GraphError)?;

    let mut zanzibar_client = ZanzibarClient::new(spicedb_client);
    zanzibar_client.seed().await.change_context(GraphError)?;

    match args.command {
        SnapshotCommand::Dump(_) => {
            pool.dump_snapshot(
                FramedWrite::new(
                    io::BufWriter::new(io::stdout()),
                    codec::bytes::JsonLinesEncoder::default(),
                ),
                &NoAuthorization,
                10_000,
            )
            .change_context(GraphError)
            .attach_printable("Failed to produce snapshot dump")?;

            tracing::info!("Snapshot dumped successfully");
        }
        SnapshotCommand::Restore(args) => {
            SnapshotStore::new(
                pool.acquire(NoAuthorization, None)
                    .await
                    .change_context(GraphError)
                    .map_err(|report| {
                        tracing::error!(error = ?report, "Failed to acquire database connection");
                        report
                    })?,
            )
            .restore_snapshot(
                FramedRead::new(
                    io::BufReader::new(io::stdin()),
                    codec::bytes::JsonLinesDecoder::default(),
                ),
                10_000,
                !args.skip_validation,
            )
            .await
            .change_context(GraphError)
            .attach_printable("Failed to restore snapshot")?;

            tracing::info!("Snapshot restored successfully");
        }
        SnapshotCommand::Reinsert => {
            let store = pool
                .acquire(NoAuthorization, None)
                .await
                .change_context(GraphError)
                .map_err(|report| {
                    tracing::error!(error = ?report, "Failed to acquire database connection");
                    report
                })?;
            let entity_types = store
                .get_entity_types(
                    AccountId::new(Uuid::new_v4()),
                    GetEntityTypesParams {
                        filter: Filter::All(vec![]),
                        temporal_axes: serde_json::from_value(json!({
                          "pinned": {
                            "axis": "transactionTime",
                            "timestamp": null
                          },
                          "variable": {
                            "axis": "decisionTime",
                            "interval": {
                              "start": null,
                              "end": null
                            }
                          }
                        }))
                        .expect("Failed to parse temporal axes"),
                        include_drafts: true,
                        after: None,
                        limit: None,
                        include_count: false,
                    },
                )
                .await
                .change_context(GraphError)?
                .entity_types;
            dbg!(entity_types.len());
            println!("Reinserting snapshot entries");
        }
    }

    Ok(())
}

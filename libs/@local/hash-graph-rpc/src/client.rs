use std::{marker::PhantomData, net::SocketAddrV4};

use error_stack::{Result, ResultExt};
use libp2p::{multiaddr::Protocol, Multiaddr};
use thiserror::Error;
use uuid::Uuid;

use crate::{
    harpc::{
        procedure::RemoteProcedure,
        service::Service,
        transport::{
            client::{ClientTransportConfig, ClientTransportLayer},
            message::{
                actor::ActorId,
                request::{Request, RequestFlags, RequestHeader},
                response::{ResponseError, ResponsePayload},
                size::PayloadSize,
                version::{TransportVersion, Version},
            },
            TransportConfig,
        },
        Decode, Encode,
    },
    types::Includes,
};

#[derive(Debug, Copy, Clone, Error)]
pub enum TransportError {
    #[error("connection has been closed by the remote peer")]
    ConnectionClosed,
    #[error("unknown protocol version")]
    InvalidTransportVersion,
    #[error("mismatch between reported payload size and actual payload size")]
    InvalidPayloadSize,
    #[error("invalid payload")]
    InvalidPayload,
}

#[derive(Debug, Copy, Clone, Error)]
pub enum RoutingError {
    #[error("unknown service")]
    UnknownService,
    #[error("service found, but request version unknown")]
    UnknownServiceVersion,
    #[error("unknown procedure")]
    UnknownProcedure,
}

#[derive(Debug, Copy, Clone, Error)]
pub enum ClientError {
    #[error("transport error: {0}")]
    Transport(TransportError),
    #[error("routing error: {0}")]
    Routing(RoutingError),
    // TODO: add these errors correctly
    #[error("unable to encode request")]
    EncodeRequest,
    #[error("unable to encode response")]
    EncodeResponse,
    #[error("unable to decode request")]
    DecodeRequest,
    #[error("unable to decode response")]
    DecodeResponse,
    #[error("timeout while waiting for response")]
    Timeout,
    #[error("internal error")]
    Internal,
    #[error("unknown error")]
    Unknown,
}

impl From<ResponseError> for ClientError {
    fn from(value: ResponseError) -> Self {
        match value {
            ResponseError::DeadlineExceeded => Self::Timeout,
            ResponseError::ConnectionClosed => Self::Transport(TransportError::ConnectionClosed),
            ResponseError::UnknownServiceVersion => {
                Self::Routing(RoutingError::UnknownServiceVersion)
            }
            ResponseError::UnknownService => Self::Routing(RoutingError::UnknownService),
            ResponseError::UnknownProcedure => Self::Routing(RoutingError::UnknownProcedure),
            ResponseError::InvalidTransportVersion => {
                Self::Transport(TransportError::InvalidTransportVersion)
            }
            ResponseError::InvalidPayloadSize => {
                Self::Transport(TransportError::InvalidPayloadSize)
            }
            ResponseError::InvalidPayload => Self::Transport(TransportError::InvalidPayload),
            ResponseError::EncodingError => Self::EncodeResponse,
            ResponseError::DecodingError => Self::DecodeRequest,
        }
    }
}

pub struct Client<S, C> {
    _service: PhantomData<S>,
    context: C,
    transport: ClientTransportLayer,
}

impl<S, C> Client<S, C>
where
    S: Service,
{
    pub fn new(context: C, remote: SocketAddrV4, config: TransportConfig) -> Self {
        Self {
            _service: PhantomData,
            context,
            transport: ClientTransportLayer::new(ClientTransportConfig {
                remote: Multiaddr::from(*remote.ip()).with(Protocol::Tcp(remote.port())),
                transport: config,
            })
            .unwrap(),
        }
    }

    pub async fn call<P>(&self, request: P) -> Result<P::Response, ClientError>
    where
        P: RemoteProcedure,
        S::Procedures: Includes<P>,
        C: Encode<P> + Decode<P::Response>,
    {
        let request = self
            .context
            .encode(request)
            .await
            .change_context(ClientError::EncodeRequest)?;

        let request = Request {
            header: RequestHeader {
                flags: RequestFlags::new(),
                version: Version {
                    transport: TransportVersion::new(0x00),
                    service: S::VERSION,
                },
                service: S::ID,
                procedure: P::ID,
                actor: ActorId::from(Uuid::nil()),
                size: PayloadSize::len(&request),
            },
            body: request,
        };

        let response = self
            .transport
            .call(request)
            .await
            .change_context(ClientError::Internal)?;

        match response.body {
            ResponsePayload::Success(body) => self
                .context
                .decode(body)
                .await
                .change_context(ClientError::DecodeResponse),
            ResponsePayload::Error(error) => panic!("error: {:?}", error),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        convert::Infallible,
        future::Future,
        net::{Ipv4Addr, SocketAddrV4},
    };

    use authorization::schema::AccountGroupPermission;
    use bytes::Bytes;
    use graph_types::account::{AccountGroupId, AccountId};
    use uuid::Uuid;

    use crate::{
        client::Client,
        harpc::transport::TransportConfig,
        specification::account::{
            AccountService, AddAccountGroupMember, CheckAccountGroupPermission, CreateAccount,
        },
    };

    struct DifferentProcedure;
    impl crate::harpc::procedure::RemoteProcedure for DifferentProcedure {
        type Response = ();

        const ID: crate::harpc::procedure::ProcedureId =
            crate::harpc::procedure::ProcedureId::derive("DifferentProcedure");
    }

    #[derive(Debug, Copy, Clone)]
    struct NullContext;
    impl crate::harpc::Stateful for NullContext {
        type State = ();

        fn state(&self) -> &Self::State {
            &()
        }
    }

    impl<T> crate::harpc::Encode<T> for NullContext {
        type Error = Infallible;

        fn encode(
            &self,
            value: T,
        ) -> impl Future<Output = error_stack::Result<Bytes, Self::Error>> + Send + 'static
        {
            async move { Ok(Bytes::new()) }
        }
    }

    impl<T> crate::harpc::Decode<T> for NullContext {
        type Error = Infallible;

        fn decode(
            &self,
            bytes: Bytes,
        ) -> impl Future<Output = error_stack::Result<T, Self::Error>> + Send + 'static {
            async move { panic!("decode") }
        }
    }

    impl crate::harpc::Context for NullContext {}

    // Compile test
    async fn _never_called() {
        let client = Client::<AccountService, _>::new(
            NullContext,
            SocketAddrV4::new(Ipv4Addr::LOCALHOST, 0),
            TransportConfig::default(),
        );

        let response = client.call(CreateAccount).await;

        let response = client
            .call(AddAccountGroupMember {
                account_group_id: AccountGroupId::new(Uuid::new_v4()),
                account_id: AccountId::new(Uuid::new_v4()),
            })
            .await;

        let response = client
            .call(CheckAccountGroupPermission {
                account_group_id: AccountGroupId::new(Uuid::new_v4()),
                permission: AccountGroupPermission::AddMember,
            })
            .await;

        // let response = client.call(DifferentProcedure).await;
    }
}

import type { PropertyType, VersionedUrl } from "@blockprotocol/type-system";
import type { EntityType } from "@blockprotocol/type-system/slim";
import { Entity } from "@local/hash-graph-sdk/entity";
import type {
  EntityId,
  EntityMetadata,
  EntityRecordId,
  PropertyObject,
} from "@local/hash-graph-types/entity";
import type { PersistedEntity } from "@local/hash-isomorphic-utils/flows/types";
import { generateEntityLabel } from "@local/hash-isomorphic-utils/generate-entity-label";
import { stringifyPropertyValue } from "@local/hash-isomorphic-utils/stringify-property-value";
import type {
  EntityRootType,
  EntityTypeRootType,
  Subgraph,
} from "@local/hash-subgraph";
import {
  getEntityTypeById,
  getPropertyTypesForEntityType,
} from "@local/hash-subgraph/stdlib";
import { extractBaseUrl } from "@local/hash-subgraph/type-system-patch";
import type { SxProps, Theme } from "@mui/material";
import { Box, Stack, TableCell, Typography } from "@mui/material";
import { memo, useMemo, useState } from "react";

import { ValueChip } from "../../../../shared/value-chip";
import type {
  CreateVirtualizedRowContentFn,
  VirtualizedTableColumn,
  VirtualizedTableRow,
  VirtualizedTableSort,
} from "../../../../shared/virtualized-table";
import {
  defaultCellSx,
  VirtualizedTable,
} from "../../../../shared/virtualized-table";
import type { ProposedEntityOutput } from "../shared/types";
import { EmptyOutputBox } from "./shared/empty-output-box";
import { outputIcons } from "./shared/icons";
import { OutputContainer } from "./shared/output-container";

const fixedFieldIds = ["status", "entityTypeId", "entityLabel"] as const;

type FixedFieldId = (typeof fixedFieldIds)[number];

const isFixedField = (fieldId: string): fieldId is FixedFieldId =>
  fixedFieldIds.includes(fieldId as FixedFieldId);

/**
 * The columns are either the fixed fields or properties of the type(s) in the table
 */
type FieldId = FixedFieldId | VersionedUrl;

type EntityColumnMetadata = { appliesToEntityTypeIds: VersionedUrl[] };

const generateColumns = (
  entityTypes: EntityType[],
  subgraph?: Subgraph,
): VirtualizedTableColumn<FieldId, EntityColumnMetadata>[] => {
  const propertyTypesByVersionedUrl: Record<
    VersionedUrl,
    PropertyType & { appliesToEntityTypeIds: VersionedUrl[] }
  > = {};

  if (subgraph) {
    for (const entityType of entityTypes) {
      const entityPropertyTypes = getPropertyTypesForEntityType(
        entityType,
        subgraph,
      ).values();

      for (const { schema } of entityPropertyTypes) {
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        propertyTypesByVersionedUrl[schema.$id] ??= {
          ...schema,
          appliesToEntityTypeIds: [],
        };

        propertyTypesByVersionedUrl[schema.$id]!.appliesToEntityTypeIds.push(
          entityType.$id,
        );
      }
    }
  }

  return [
    {
      label: "Status",
      id: "status",
      sortable: true,
      width: 100,
    },
    {
      label: "Type",
      id: "entityTypeId",
      sortable: true,
      width: 120,
    },
    {
      label: "Label",
      id: "entityLabel",
      sortable: true,
      width: "auto",
    },
    ...Object.values(propertyTypesByVersionedUrl)
      .sort((a, b) => a.title.localeCompare(b.title))
      .map((property) => ({
        label: property.title,
        id: property.$id,
        sortable: true,
        width: "auto",
        metadata: { appliesToEntityTypeIds: property.appliesToEntityTypeIds },
      })),
  ];
};

type EntityResultRow = {
  entityLabel: string;
  entityTypeId: VersionedUrl;
  entityType: EntityType;
  onEntityClick: (entity: Entity) => void;
  onEntityTypeClick: (entityTypeId: VersionedUrl) => void;
  persistedEntity?: Entity;
  properties: PropertyObject;
  researchOngoing: boolean;
  status: "Proposed" | "Created" | "Updated";
};

const typographySx = {
  color: ({ palette }) => palette.common.black,
  fontSize: 12,
  fontWeight: 500,
} as const satisfies SxProps<Theme>;

const cellSx = {
  ...defaultCellSx,
  ...typographySx,
  "&:not(:last-child)": {
    borderRight: ({ palette }) => `1px solid ${palette.gray[20]}`,
  },
} as const satisfies SxProps<Theme>;

const TableRow = memo(
  ({
    columns,
    row,
  }: {
    columns: VirtualizedTableColumn<FieldId, EntityColumnMetadata>[];
    row: EntityResultRow;
  }) => {
    const entityTypeTitle = row.entityType.title;

    return (
      <>
        <TableCell sx={cellSx}>{row.status}</TableCell>
        <TableCell sx={{ ...cellSx, px: 0.5 }}>
          <Box
            component="button"
            onClick={() => row.onEntityTypeClick(row.entityTypeId)}
            sx={{ background: "none", border: "none", p: 0 }}
          >
            <ValueChip
              type
              sx={{
                cursor: "pointer",
                ml: 1,
                ...typographySx,
              }}
            >
              {entityTypeTitle}
            </ValueChip>
          </Box>
        </TableCell>
        <TableCell sx={cellSx}>
          {row.persistedEntity ? (
            <Box
              component="button"
              onClick={() => row.onEntityClick(row.persistedEntity!)}
              sx={{
                background: "none",
                border: "none",
                cursor: "pointer",

                p: 0,
                textAlign: "left",
              }}
            >
              <ValueChip
                sx={{
                  ...typographySx,
                  color: ({ palette }) => palette.blue[70],
                }}
              >
                {row.entityLabel}
              </ValueChip>
            </Box>
          ) : (
            <ValueChip sx={typographySx}>{row.entityLabel}</ValueChip>
          )}
        </TableCell>
        {columns.slice(fixedFieldIds.length).map((column) => {
          const appliesToEntity = column.metadata?.appliesToEntityTypeIds.some(
            (id) => id === row.entityTypeId,
          );

          if (!appliesToEntity) {
            return (
              <TableCell
                sx={({ palette }) => ({
                  ...cellSx,
                  background: palette.gray[5],
                  color: palette.gray[50],
                })}
                key={column.id}
              >
                Does not apply
              </TableCell>
            );
          }

          const entityValue =
            row.properties[extractBaseUrl(column.id as VersionedUrl)];

          if (entityValue === undefined || entityValue === "") {
            if (row.researchOngoing) {
              return (
                <TableCell
                  key={column.id}
                  sx={({ palette }) => ({
                    ...cellSx,
                    background: palette.blue[15],
                    color: palette.blue[70],
                  })}
                >
                  <Stack direction="row" alignItems="center">
                    <Box
                      sx={{
                        background: ({ palette }) => palette.blue[70],
                        height: 6,
                        width: 6,
                        borderRadius: "50%",
                        mr: 1,
                      }}
                    />
                    Researching...
                  </Stack>
                </TableCell>
              );
            }
            return (
              <TableCell
                key={column.id}
                sx={{ ...cellSx, color: ({ palette }) => palette.gray[50] }}
              >
                –
              </TableCell>
            );
          }

          const value = stringifyPropertyValue(entityValue);

          return (
            <TableCell key={column.id} sx={cellSx}>
              {!!value.length && (
                <ValueChip tooltip={value} sx={{ maxWidth: 700 }}>
                  <Typography
                    sx={{
                      ...typographySx,
                      maxWidth: "100%",
                      lineHeight: 1,
                      textOverflow: "ellipsis",
                      overflow: "hidden",
                      whiteSpace: "nowrap",
                    }}
                  >
                    {value}
                  </Typography>
                </ValueChip>
              )}
            </TableCell>
          );
        })}
      </>
    );
  },
);

const createRowContent: CreateVirtualizedRowContentFn<
  EntityResultRow,
  FieldId,
  EntityColumnMetadata
> = (_index, row, context) => (
  <TableRow columns={context.columns} row={row.data} />
);

type EntityResultTableProps = {
  onEntityClick: (entity: Entity) => void;
  onEntityTypeClick: (entityTypeId: VersionedUrl) => void;
  persistedEntities: PersistedEntity[];
  persistedEntitiesSubgraph?: Subgraph<EntityRootType>;
  proposedEntities: ProposedEntityOutput[];
  proposedEntitiesTypesSubgraph?: Subgraph<EntityTypeRootType>;
};

export const EntityResultTable = ({
  onEntityClick,
  onEntityTypeClick,
  persistedEntities,
  persistedEntitiesSubgraph,
  proposedEntities,
  proposedEntitiesTypesSubgraph,
}: EntityResultTableProps) => {
  const [sort, setSort] = useState<VirtualizedTableSort<FieldId>>({
    field: "entityLabel",
    direction: "asc",
  });

  const hasData = !!(persistedEntities.length || proposedEntities.length);

  const {
    rows,
    entityTypes,
  }: {
    rows: VirtualizedTableRow<EntityResultRow>[];
    entityTypes: EntityType[];
  } = useMemo(() => {
    const rowData: VirtualizedTableRow<EntityResultRow>[] = [];
    const entityTypesById: Record<VersionedUrl, EntityType> = {};

    for (const record of persistedEntities.length
      ? persistedEntities
      : proposedEntities) {
      const isProposed = "localEntityId" in record;

      const entity = isProposed
        ? record
        : record.entity
          ? new Entity(record.entity)
          : undefined;

      if (!entity) {
        continue;
      }

      const entityId =
        "localEntityId" in entity
          ? entity.localEntityId
          : entity.metadata.recordId.entityId;

      const entityTypeId =
        "entityTypeId" in entity
          ? entity.entityTypeId
          : entity.metadata.entityTypeId;

      const entityLabel = generateEntityLabel(
        persistedEntitiesSubgraph ?? null,
        {
          properties: entity.properties,
          metadata: {
            recordId: {
              editionId: "irrelevant-here",
              entityId: `ownedBy~${entityId}` as EntityId,
            } satisfies EntityRecordId,
            entityTypeId: entityTypeId satisfies VersionedUrl,
          } as EntityMetadata,
        },
      );

      const subgraph = isProposed
        ? proposedEntitiesTypesSubgraph
        : persistedEntitiesSubgraph;

      if (!subgraph) {
        continue;
      }

      let entityType = entityTypesById[entityTypeId];
      if (!entityType) {
        const entityTypeWithMetadata = getEntityTypeById(
          subgraph,
          entityTypeId,
        );

        if (!entityTypeWithMetadata) {
          // The data for the types may not arrive at the same time as the proposal
          continue;
        }

        entityType = entityTypeWithMetadata.schema;
        entityTypesById[entityTypeId] = entityType;
      }

      rowData.push({
        id: entityId,
        data: {
          entityLabel,
          entityTypeId,
          entityType,
          onEntityClick,
          onEntityTypeClick,
          persistedEntity: "metadata" in entity ? entity : undefined,
          properties: entity.properties,
          researchOngoing:
            "researchOngoing" in record && record.researchOngoing,
          status: isProposed
            ? "Proposed"
            : record.operation === "update"
              ? "Updated"
              : "Created",
        },
      });
    }

    return {
      entityTypes: Object.values(entityTypesById),
      rows: rowData.sort((a, b) => {
        const field = sort.field;
        const direction = sort.direction === "asc" ? 1 : -1;

        if (!isFixedField(field)) {
          /**
           * This is a property field, so we need to compare the values of the properties
           */
          const baseUrl = extractBaseUrl(field);

          return (
            (a.data.properties[baseUrl]
              ?.toString()
              .localeCompare(b.data.properties[baseUrl]?.toString() ?? "") ??
              0) * direction
          );
        }

        return a.data[field].localeCompare(b.data[field]) * direction;
      }),
    };
  }, [
    onEntityClick,
    onEntityTypeClick,
    persistedEntities,
    persistedEntitiesSubgraph,
    proposedEntities,
    proposedEntitiesTypesSubgraph,
    sort,
  ]);

  const columns = useMemo(
    () =>
      generateColumns(
        entityTypes,
        persistedEntities.length === 0
          ? proposedEntitiesTypesSubgraph
          : persistedEntitiesSubgraph,
      ),
    [
      entityTypes,
      proposedEntitiesTypesSubgraph,
      persistedEntitiesSubgraph,
      persistedEntities.length,
    ],
  );

  return (
    <OutputContainer
      noBorder={hasData}
      sx={{
        flex: 1,
        minWidth: 400,
        "& table": {
          tableLayout: "auto",
        },
        "& th:not(:last-child)": {
          borderRight: ({ palette }) => `1px solid ${palette.gray[20]}`,
        },
      }}
    >
      {hasData ? (
        <VirtualizedTable
          columns={columns}
          createRowContent={createRowContent}
          rows={rows}
          sort={sort}
          setSort={setSort}
        />
      ) : (
        <EmptyOutputBox
          Icon={outputIcons.table}
          label="Entities proposed and affected by this flow will appear in a table here"
        />
      )}
    </OutputContainer>
  );
};

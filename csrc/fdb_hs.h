#ifndef FDB_HS_H
#define FDB_HS_H

#define FDB_API_VERSION 510
#include <foundationdb/fdb_c.h>
#include <foundationdb/fdb_c_options.g.h>

int fdb_hs_select_api_version(void);

FDBFuture* fdb_hs_cluster_create_database(FDBCluster *cluster);

#endif // FDB_HS_H

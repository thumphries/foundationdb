
#include "fdb_hs.h"

int fdb_hs_select_api_version(void) {
  return fdb_select_api_version(FDB_API_VERSION);
}

FDBFuture* fdb_hs_cluster_create_database(FDBCluster *cluster) {
  return fdb_cluster_create_database(cluster, (const uint8_t *) "DB", 2);
}

#define FDB_API_VERSION 510
#include <foundationdb/fdb_c.h>
#include <foundationdb/fdb_c_options.g.h>

#include "fdb_hs.h"

int fdb_hs_select_api_version(void) {
  return fdb_select_api_version(FDB_API_VERSION);
}
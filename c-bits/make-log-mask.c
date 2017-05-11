#include <syslog.h>

int makeLogMask(int priority)
{
  return LOG_MASK(priority);
}

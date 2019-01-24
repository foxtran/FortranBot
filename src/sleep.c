#define _POSIX_C_SOURCE 199309L

#include <time.h>

void csleep_(int *milliseconds)
{
  int ms_remaining = (*milliseconds) % 1000;
  long usec = ms_remaining * 1000;
  struct timespec ts_sleep;

  ts_sleep.tv_sec = (*milliseconds) / 1000;
  ts_sleep.tv_nsec = 1000*usec;
  nanosleep(&ts_sleep, NULL);
}
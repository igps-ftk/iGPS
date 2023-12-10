#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

  // read a character from keyboard
  // written by David Hollinger, 2009

int charavail_() {
  int cnt;
  int error;
  char c;
  char x;
  
  // get current state of the STDIN flags
  int flags = fcntl(STDIN_FILENO, F_GETFL);

  // set STDIN to nonblocking
  fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK);

  // see if we can read
  cnt = read(0,&c,1);
  // record errno before we reset STDIN
  error = errno;

  // figure out if there is data available
  if ((cnt==-1) && (errno==EAGAIN)) {
    // reset STDIN back to normal
    fcntl(0,F_SETFL,flags);
    return(0); // no data - just return 0
  }
  //  read the newline as well, ignore errors, etc.
  read(0,&x,1);
  fcntl(0,F_SETFL,flags);
  return(c);
}

/*
 test main().
  
int main() {
  int i;
  
  while (! charavail_()) {
    for (i=0;i<10000000;i++) ;
  }
  
  printf("INTERRUPTED\n");
  return(0);
}

*/

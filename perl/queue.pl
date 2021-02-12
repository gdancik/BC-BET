#!/usr/bin/perl
#############################################################
# make sure server is not busy
#############################################################
sub busy () {
  my $numjobs = `ps -ef | grep /usr/lib/R/bin/exec/R | wc -l`;
#  print "<html><body><p> HELLO $numjobs </p></body></html>";
  if ($numjobs > 5) {
   
#    print "<html><head><meta http-equiv=\"refresh\" content=\"10\"></head>";
    print "<html><body>"; 
    print "<p style = \"background-color: lightgreen; font-size:120\%; font-weight: bold; font-color:white\">";
    print "<br>Sorry, the server is currently experiencing a very high load. Please try again later. </p>";
    print "<p> <a href = \"../BCBET2\"> BACK </a>";
    print "</body></html>";

    return 1;
  } 
  return 0;
}
1;

#include <iostream>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/posix_time/posix_time_io.hpp>

using namespace boost::posix_time;
using namespace std;

int main(int argc, char **argv) {
  time_facet *facet = new time_facet("%d-%b-%Y %H:%M:%S");
  cout.imbue(locale(cout.getloc(), facet));
  cout << second_clock::local_time() << endl;
}


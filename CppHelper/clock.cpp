// util/chrono1.cpp

#include <chrono>
#include <ctime>
#include <string>
#include <iostream>

using namespace std;

string asString (void)
{
     // convert to system time:
     time_t t = chrono::system_clock::to_time_t(chrono::system_clock::now());
     string ts = std::ctime(&t);// convert to calendar time
     ts.resize(ts.size()-1);         // skip trailing newline
     return ts;
}

int main()
{
     cout << asString() << std::endl;
}


#include <iostream>
#include <memory>

using namespace std;

unique_ptr<int> foo()
{
	return unique_ptr<int>(new int(10));
}

int main()
{
	unique_ptr<int> p = foo();

	cout << *p << endl;
	return 0;
}

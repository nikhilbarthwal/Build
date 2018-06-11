//============================================================================
// Name        : Test2.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <type_traits>
#include <iostream>
#include "Base/x.h"
#include <memory>
#include <string>
#include <vector>

using namespace std;

class Interface
{
public:
	string i;
	Interface(string k)
	{
		i = k;
	}

	virtual unique_ptr<Interface> clone() const = 0;
	virtual string Str() = 0;
	virtual ~Interface()
	{
	}

	void I()
	{
		cout << i << endl;
	}
};

class Object: public Interface
{
private:
	unique_ptr<Interface> obj;
public:

	Object(const Object& x) :
			Interface("Object")
	{
		obj = std::move(x.clone());
	}

	Object(const Interface& x) :
			Interface("Object")
	{
		obj = std::move(x.clone());
	}
	unique_ptr<Interface> clone() const
	{
		return unique_ptr<Interface>(new Object(*obj));
	}

	string Str()
	{
		return obj->Str();
	}

};

class Foo: public Interface
{
private:

public:
	Foo() :
			Interface("Foo")
	{
	}

	virtual string Str(void)
	{
		return "Foo";
	}
	unique_ptr<Interface> clone() const
	{
		return unique_ptr<Interface>(new Foo(*this));
	}
};

class Bar: public Interface
{
private:
	const string s = "Bar";
public:

	Bar() :
			Interface("Bar")
	{
	}

	virtual string Str(void)
	{
		return "Bar";
	}
	unique_ptr<Interface> clone() const
	{
		return unique_ptr<Interface>(new Bar(*this));
	}
};

template<typename T1, typename T2>
class BasePair
{
	static_assert(std::is_base_of<Interface, T1>::value, "T1 must derive from Base");
	static_assert(std::is_base_of<Interface, T2>::value, "T2 must derive from Base");

	T1 first;
	T2 second;
};

int main()
{
	vector<Object> v;
	Object O1 = Bar();
	Object O2 = (Foo());
	auto O = O2.clone();
	v.push_back(Foo());
	cout << O1.Str() << endl;
	cout << O2.Str() << endl;
	O1.I();
	Object O3 = O2;
	cout << O3.Str() << endl;
	cout << t << endl;
	return 0;
}

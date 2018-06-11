template <class T>
class ReadOnly<T>
{
private:
	unique_ptr<T> obj;
public:
	void init(const T& x)
	{
		obj = 
	}
};


class Foo
{
private:
    int i;

public:
    Foo(int j) :i(j)     { cout << "Foo::Foo\n";  }
    ~Foo()     { cout << "Foo::~Foo\n"; }
    void bar() { cout << "Foo::bar  "<<i<<"\n";  }
};
 
 
int main()
{
    const unique_ptr<Foo> p1(new Foo(5));  // p1 owns Foo
    unique_ptr<Foo> p2;
    p2->bar();
    if (p2) p1->bar();
    {
        cout<<"Yes"<<endl;
    }
    return 0;
}

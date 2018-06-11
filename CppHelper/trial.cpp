#include <iostream>
#include <vector>


using namespace std;

class Z
{
private:
vector<int> x;

public:
    Z(vector<int> X)
    {
        //x(X.size());
        for( auto& i:X)
        {
            cout<<i<<endl;
            X.push_back(i);
        };

    }
};

const Z z = Z({1,3});

const auto c = {1,2,3,4,5};


int main(void)
{
    cout<<"main"<<c.size()<<endl;
    for( const auto& i:c) { cout<<i<<endl; }
        //cout<<c[3]<<endl;
    return 0;
}


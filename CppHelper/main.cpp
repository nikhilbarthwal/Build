// ***** Main.cpp: Main entry point  *****

#include <iostream>
#include "utils.h"

using namespace std;

int main(void)
{
    cout << "Hello World!" << endl;
    return 0;
}

/*

 #include <msclr/marshal_cppstd.h>
 #include <string>
 #include <iostream>



 int main(array<System::String ^> ^args)
 {
 #ifdef WINDOWS
 string s1 = msclr::interop::marshal_as< std:
 :string >((Guid::NewGuid()).ToString());

 string s2 = msclr::interop::marshal_as< std:
 :string >(((gcnew DateTime())->Now).ToString("F"));
 #endif

 cout << s1 << endl;
 cout << s2 << endl;

 string ss1 = "Dummy1";
 string ss2 = "Dummy2";

 #ifdef WINDOWS
 auto SS1 = gcnew String(ss1.c_str());
 auto SS2 = gcnew String(ss2.c_str());

 Directory::CreateDirectory(SS1);
 auto SS = Path::Combine(SS1, SS2);
 Directory::CreateDirectory(SS);
 #endif

 return 0;
 }*/


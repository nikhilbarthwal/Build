// ***** utils.h: Utility Header file  *****

#include <iostream>
#include <vector>
#include <fstream>
#include "config.h"

#ifndef BUILD_UTILS
#define BUILD_UTILS

using namespace std;

typedef vector<string> text;
typedef vector<int> numbers;

int Max(numbers n)
{
    int l = n.size();
    if (l == 0)
    {
        cerr << "ERROR: Maximum cannot be computed for empty list of numbers" << endl;
        throw 0;
    }

    int m = n[0];
    for (int i = 0; i < l; i++)
    {
        int k = n[i];
        if (k > m) m = k;
    }
    return m;
}

int Min(numbers n)
{
    int l = n.size();
    if (l == 0)
    {
        cerr << "ERROR: Maximum cannot be computed for emty list of numbers" << endl;
        throw 0;
    }

    int m = n[0];
    for (int i = 0; i < l; i++)
    {
        int k = n[i];
        if (m > k) m = k;
    }
    return m;
}

class Config
{
    private:
        static string timeStamp;
        static string id;

    public:
        static string TimeStamp(void)
        {
            return Config::timeStamp;
        }

        static string Id(void)
        {
            return Config::id;
        }

        static bool Ok(void)
        {
            if ((Config::id).empty() || (Config::timeStamp).empty()) return false;
            else return true;
        }

        static void Init(void)
        {
            Config::timeStamp = "Now"; // TODO: Generate timeStamp
            Config::id = "0"; // TODO: Generate id
            cout << "Initializing Builder .." << endl << endl;
            cout << "Build Id: " << (Config::id) << endl;
            cout << "Time Stamp: " << (Config::timeStamp) << endl;
        }
};

bool IsLower(char c)
{
    if ((c >= 'a') && (c <= 'z')) return true;
    else return false;
}

bool IsUpper(char c)
{
    if ((c >= 'A') && (c <= 'Z')) return true;
    else return false;
}

bool IsDigit(char c)
{
    if ((c >= '0') && (c <= '9')) return true;
    else return false;
}

bool IsAlpha(char c)
{
    if (IsLower(c) || IsUpper(c)) return true;
    else return false;
}

bool IsWhite(char c)
{
    if ((c = 9) || (c = 10)) return true;
    else return false;
}

bool IsAlphaNum(char c)
{
    if (IsAlpha(c) || IsDigit(c)) return true;
    else return false;
}

bool IsIdentifier(string s)
{
    int n = s.size();
    if (n == 0) return false;
    if (!IsAlpha(s[0])) return false;
    for (int i = 1; i < n; i++)
        if (!IsAlphaNum(s[i])) return false;
    return true;
}

char ToLower(char c)
{
    if ((c >= 'A') && (c <= 'Z')) return (c - 'A' + 'a');
    else return c;
}

char ToUpper(char c)
{
    if ((c >= 'a') && (c <= 'z')) return (c - 'a' + 'A');
    else return c;
}

string ToLower(string s)
{
    int n = s.size();
    string ss = s;
    for (int i = 0; i < n; i++)
        ss[i] = ToLower(s[i]);
    return ss;
}

string ToUpper(string s)
{
    int n = s.size();
    string ss = s;
    for (int i = 0; i < n; i++)
        ss[i] = ToUpper(s[i]);
    return ss;
}

string CamelIdentifier(string s)
{
    if (!IsIdentifier(s))
    {
        cerr << "ERROR: Expecting " << s << " to be a identifier" << endl;
        throw 0;
    }
    string ss = ToLower(s);
    return ss;
}

string PascalIdentifier(string s)
{
    if (!IsIdentifier(s))
    {
        cerr << "ERROR: Expecting " << s << " to be a identifier" << endl;
        throw 0;
    }
    string ss = ToLower(s);
    ss[0] = ToUpper(ss[0]);
    return ss;
}

string chr2str(char c)
{
    string s(" ");
    s[0] = c;
    return s;
}

string Repeat(string s, int n)
{
    string ss;
    for (int i = 0; i < n; i++)
        ss += s;
    return ss;
}

int Half(int n)
{
    int k = n % 2;
    n -= k;
    return (int) (n / 2);
}

class W
{
    protected:
        string s;
        string ss;
        void check(string z)
        {
            if (!IsIdentifier(z))
            {
                cerr << "ERROR: Expecting " << z << " to be a identifier" << endl;
                throw 0;
            }
        }

    public:
        W(void)
        {
            cerr << "ERROR: Direct use of W is  not permitted. Use W1/W2/W3/W4/W5" << endl;
            throw 0;
        }
        ;

        string Camel(void)
        {
            return (CamelIdentifier(s) + ss);
        }

        string Pascal(void)
        {
            return (PascalIdentifier(s) + ss);
        }
};

class W1 : public W
{
    public:
        W1(string s1)
        {
            check(s1);
            s = s1;
            ss = "";
        }
};

class W2 : public W
{
    public:
        W2(string s1, string s2)
        {
            check(s1);
            check(s2);
            s = s1;
            ss = PascalIdentifier(s2);
            return;
        }
};

class W3 : public W
{
    public:
        W3(string s1, string s2, string s3)
        {
            check(s1);
            W2 w = W2(s2, s3);
            s = s1;
            ss = w.Pascal();
            return;
        }
};

class W4 : public W
{
    public:
        W4(string s1, string s2, string s3, string s4)
        {
            check(s1);
            s = s1;
            W3 w = W3(s2, s3, s4);
            ss = w.Pascal();
            return;
        }
};

class W5 : public W
{
    public:
        W5(string s1, string s2, string s3, string s4, string s5)
        {
            check(s1);
            s = s1;
            W4 w = W4(s2, s3, s4, s5);
            ss = w.Pascal();
            return;
        }
};

class Line
{
    protected:
        string ty;
        Line(string s)
        {
            ty = s;
        }

    public:
        Line(void)
        {
            ty = "Line";
            return;
        }

        string Type(void)
        {
            return ty;
        }

        int Length(void)
        {
            return 0;
        }

        string ToString(void)
        {
            return "";
        }

        string ToString(int)
        {
            return "";
        }

        virtual Line Indent(string)
        {
            return Line();
        }

        virtual ~Line(void)
        {
        }
        ;
};

class LineMulti : public Line
{
    private:
        string s;
        string t1;
        string t2;
        char c;

    public:
        LineMulti(string ss, string tt1, string tt2)
        {
            ty = "LineMulti";
            s = ss;
            t1 = tt1;
            t2 = tt2;
            c = ' ';
            return;
        }

        LineMulti(string ss, string tt1, string tt2, char ch)
        {
            ty = "LineMulti";
            s = ss;
            t1 = tt1;
            t2 = tt2;
            c = ch;
            return;
        }

        int Length(void)
        {
            return (2 + t1.length() + t2.length() + s.length());
        }

        string ToString(void)
        {
            return (t1 + chr2str(c) + s + chr2str(c) + t2);
        }

        string ToString(int n)
        {

            int n1 = t1.length();
            int n2 = t2.length();
            int k = n - n1 - n2;

            if (k > 1)
            {
                int k1 = Half(k);
                int k2 = k - k1;

                string ss1 = Repeat(chr2str(c), k1);
                string ss2 = Repeat(chr2str(c), k2);

                return (t1 + ss1 + s + ss2 + t2);

            }
            else
            {
                cerr << "Length of LineMulti(" << s << "," << t1 << "," << t2 << ") exceeds " << n << endl;
                throw 0;
            }
        }

        Line Indent(string t)
        {
            return LineMulti(s, t1 + t, t2);
        }

};

class Line1 : public Line
{
    private:
        string s;

    public:
        Line1(string str)
        {
            ty = "Line1";
            s = str;
            return;
        }

        int Length(void)
        {
            return s.length();
        }

        string ToString(void)
        {
            return s;
        }

        string ToString(int n)
        {
            int l = s.length();
            if (l > n)
            {
                cerr << "Length of Line1(" << s << ") exceeds " << n << endl;
                throw 0;
            }

            return s;
        }

        Line Indent(string t)
        {
            return Line1(t + s);
        }

};

class Line2 : public Line
{
    private:
        string s1;
        string s2;
        char c;

    public:
        Line2(string str1, string str2)
        {
            ty = "Line2";
            s1 = str1;
            s2 = str2;
            c = ' ';
            return;
        }

        Line2(string str1, string str2, char ch)
        {
            ty = "Line2";
            s1 = str1;
            s2 = str2;
            c = ch;
            return;
        }

        int Length(void)
        {
            return (1 + s1.length() + s2.length());
        }

        string ToString(void)
        {
            return (s1 + chr2str(c) + s2);
        }

        string ToString(int n)
        {
            int n1 = s1.length();
            int n2 = s2.length();
            int k = n - n1 - n2;

            if (k > 0)
            {
                return (s1 + Repeat(chr2str(c), k) + s2);
            }
            else
            {
                cerr << "ERROR: Length of Line2(\"" << s1 << "\", \"" << s2 << "\") exceeds " << n << endl;
                throw 0;
            }
        }

        Line Indent(string t)
        {
            return Line2(s1 + t, s2);
        }

};

class LineSuffix : public Line
{
    private:
        string s;
        string t;
        char c;

    public:
        LineSuffix(string ss, string tt)
        {
            ty = "LineSuffix";
            s = ss;
            t = tt;
            c = ' ';
            return;
        }

        LineSuffix(string ss, string tt, char ch)
        {
            ty = "LineSuffix";
            s = ss;
            t = tt;
            c = ch;
            return;
        }

        int Length(void)
        {
            return (1 + s.length() + t.length());
        }

        string ToString(void)
        {
            return (s + chr2str(c) + t);
        }

        string ToString(int n)
        {
            int n1 = s.length();
            int n2 = t.length();
            int k = n - n2;

            if (k > n1)
            {
                k -= n1;
                int k1 = Half(k);
                int k2 = k - k1;

                string ss1 = Repeat(chr2str(c), k1);
                string ss2 = Repeat(chr2str(c), k2);

                return (ss1 + s + ss2 + t);
            }
            else
            {
                cerr << "ERROR: Length of LineSuffix(" << s << "," << t << ") exceeds " << n << endl;
                throw 0;
            }
        }

        Line Indent(string tt)
        {
            return LineMulti(s, tt, t);
        }

};

class LinePrefix : public Line
{
    private:
        string s;
        string t;
        char c;

    public:
        LinePrefix(string ss, string tt)
        {
            ty = "LineSuffix";
            s = ss;
            t = tt;
            c = ' ';
            return;
        }

        LinePrefix(string ss, string tt, char ch)
        {
            ty = "LineSuffix";
            s = ss;
            c = ch;
            return;
        }

        int Length(void)
        {
            return (1 + s.length() + t.length());
        }

        string ToString(void)
        {
            return (t + chr2str(c) + s);
        }

        string ToString(int n)
        {
            int n1 = s.length();
            int n2 = t.length();
            int k = n - n2;

            if (k > n1)
            {
                k -= n1;
                int k2 = Half(k);
                int k1 = k - k2;

                string ss1 = Repeat(chr2str(c), k1);
                string ss2 = Repeat(chr2str(c), k2);

                return (t + ss1 + s + ss2);
            }
            else
            {
                cerr << "ERROR: Length of LineSuffix(" << s << "," << t << "\") exceeds " << n << endl;
                throw 0;
            }
        }

        Line Indent(string i)
        {
            return LinePrefix(s, t + i);
        }
};

class LineCenter : public Line
{
    private:
        string s;
        char c;

    public:
        LineCenter(string str)
        {
            ty = "LineCenter";
            s = str;
            c = ' ';
            return;
        }

        LineCenter(string str, char ch)
        {
            ty = "LineCenter";
            s = str;
            c = ch;
            return;
        }

        int Length(void)
        {
            return s.length();
        }

        string ToString(void)
        {
            return s;
        }

        string ToString(int k)
        {
            int n = s.length();

            if (k > n)
            {
                k -= n;
                int k1 = Half(k);
                int k2 = k - k1;

                string ss1 = Repeat(chr2str(c), k1);
                string ss2 = Repeat(chr2str(c), k2);

                return ss1 + s + ss2;
            }
            else
            {
                cerr << "ERROR: Length of LineCenter(\"" << s << ") exceeds " << n << endl;
                throw 0;
            }
        }

        Line Indent(string t)
        {
            return LinePrefix(s, t);
        }

};

class Text
{
    private:
        vector<Line> z;
        text print(void); // vector string
        text print(int);

    public:
        Text();
        Text(Line);
        Text(text);
        Text(const Text&);

        int Size(void); // Number of Lines
        int Length(void); // Max of Min Length of all lines
        Line Get(int);

        void Space();
        void Space(int);

        void Add(Line);
        void Append(Text);

        void Indent(void);
        void Indent(int);

        void Print(string);
        void Print(string, int);

        void Display();
        void Display(int);

        static Text MergeText(Line s1, Line s2);
        static Text MergeText(Text t1, Line s2);
        static Text MergeText(Line s1, Text t2);
        static Text MergeText(Text t1, Text t2);
};

Text::Text(void)
{
    return;
}

Text::Text(Line l)
{
    z.push_back(l);
    return;
}

Text::Text(const Text& t)
{
    int n = t.z.size();
    for (int i = 0; i < n; i++)
        z.push_back(t.z[i]);
    return;
}

text Text::print(void)
{
    int n = this->Size();
    text t(n);

    for (int i = 0; i < n; i++)
        t.push_back((this->z[i]).ToString());
    return t;
}

text Text::print(int k)
{
    int n = this->Size();
    text t(n);

    for (int i = 0; i < n; i++)
        t.push_back((this->z[i]).ToString(k));
    return t;
}

int Text::Size(void)
{
    return ((int) z.size());
}

int Text::Length(void)
{
    int m = 0;
    for (int i = 0; i < this->Size(); i++)
    {
        string s = (z[i]).ToString();
        int k = s.length();
        if (k > m) m = k;
    }

    return m;
}

Line Text::Get(int k)
{
    int n = this->Size();
    if (k < n)
    {
        return z[k];
    }
    else
    {
        cerr << "ERROR: Index " << k << " exceeds the size of this text" << endl;
        throw 0;
    }
}

void Text::Space(void)
{
    Add(Line());
}

void Text::Space(int n)
{
    for (int i = 0; i < n; i++)
        Add(Line());
}

void Text::Add(Line l)
{
    z.push_back(l);
}

void Text::Append(Text t)
{
    for (int i = 0; i < t.Size(); i++)
        Add(t.Get(i));
}

void Text::Indent(void)
{
    Indent(1);
}

void Text::Indent(int n)
{
    string s = Repeat(Tab, n);
    for (int i = 0; i < this->Size(); i++)
    {
        z[i] = (z[i]).Indent(s);
    }
}

void Text::Print(string f)
{
    text t = this->print();
    ofstream myfile(f.c_str());
    if (myfile.is_open())
    {
        int n = this->Size();
        for (int i = 0; i < n; i++)
            myfile << t[i] << endl;
        myfile.close();
    }
    else
    {
        cerr << "Error: Unable to open file" << f << endl;
        throw exception();
    }
    return;
}

void Text::Print(string f, int k)
{
    text t = this->print(k);
    ofstream myfile(f.c_str());
    if (myfile.is_open())
    {
        int n = this->Size();
        for (int i = 0; i < n; i++)
            myfile << t[i] << endl;
        myfile.close();
    }
    else
    {
        cerr << "Error: Unable to open file" << f << endl;
        throw exception();
    }
    return;
}

void Text::Display(void)
{
    text t = this->print();
    int n = this->Size();
    for (int i = 0; i < n; i++)
        cout << t[i] << endl;
    return;
}

void Text::Display(int k)
{
    text t = this->print(k);
    int n = this->Size();
    for (int i = 0; i < n; i++)
        cout << t[i] << endl;
    return;
}

Text Text::MergeText(Text t1, Line s2)
{
    Text t(t1);
    t.Add(s2);
    return t;
}

Text Text::MergeText(Text t1, Text t2)
{
    Text t(t1);
    int n = t2.Size();
    for (int i = 0; i < n; i++)
        t.Add(t2.Get(i));
    return t;
}

Text Text::MergeText(Line s1, Text t2)
{
    int n = t2.Size();
    Text t(s1);
    for (int i = 0; i < n; i++)
        t.Add(t2.Get(i));
    return t;
}

Text Text::MergeText(Line s1, Line s2)
{
    Text t(s1);
    t.Add(s2);
    return t;
}

#endif

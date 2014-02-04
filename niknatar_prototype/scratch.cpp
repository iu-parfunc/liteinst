#include <iostream>
#include <utility>
#include <unordered_map>
#include <string>

using namespace std;

/* Compile using c++11 */

struct foo
{
  int x;

  foo(int x1): x(x1) {}
};

int main()
{
  typedef unordered_map<string, foo> map;
  map m1;
  
  foo* bar = new foo(1);
  cout << bar.x << endl;
  // foo
  //  m1.insert(map::value_type("bar", bar));
  //  cout << m1["bar"].x << endl;
  
  //  foo* f = &(m1["foo"]);
  //  f->x = 10;
  //  cout << m1["foo"].x << endl;

  //  cout << m1["foo"].second << endl;
  //  pair<const char*, int>* pr = &(m1["foo"]);
  //  pr->second = 10;
  //  cout << m1["foo"].second << endl;
  
  // cout << sizeof(struct foo) << endl;
  //  cout << sizeof(int) << endl;

  //  cout << bar.x << endl;

  int i = 5;
  int* j = &i;
  int* k = j;
  //  cout << j << endl;
  //  cout << k << endl;
  
  cout << (pair<int*, const char*>(j, "foo")).second << endl;
  
  int* l = nullptr;

  if (l == nullptr)
    cout << "yep" << endl;
}

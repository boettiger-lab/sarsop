// code edit assisted by ChatGPT 2024-06-24
#ifndef SymbolSetIterator_H
#define SymbolSetIterator_H

#include <iterator>

#include "Const.h"
using namespace std;
//using namespace momdp;

namespace momdp
{
template <typename T> class SymbolSet;

template <typename T>
class SymbolSetIterator
{
  SymbolSet<T> *parentSet;
  int symbolID;
public:
  using iterator_category = input_iterator_tag;
  using value_type = T;
  using difference_type = ptrdiff_t;
  using pointer = T*;
  using reference = T&;

  SymbolSetIterator(SymbolSet<T> *_parentSet, int _symbolID)
    : parentSet(_parentSet), symbolID(_symbolID) {}

  SymbolSetIterator(const SymbolSetIterator& mit)
    : parentSet(mit.parentSet), symbolID(mit.symbolID) {}

  SymbolSetIterator<T>& operator++()
  {
    ++symbolID;
    return *this;
  }

  SymbolSetIterator<T> operator++(int)
  {
    SymbolSetIterator<T> tmp(*this);
    ++symbolID;
    return tmp;
  }

  bool operator==(const SymbolSetIterator& rhs) const
  {
    return parentSet == rhs.parentSet && symbolID == rhs.symbolID;
  }

  bool operator!=(const SymbolSetIterator& rhs) const
  {
    return !(*this == rhs);
  }

  int operator*() const
  {
    return symbolID;
  }

  T& value() const
  {
    return parentSet->getSymbol(symbolID);
  }

  int index() const
  {
    return symbolID;
  }
};
}
#endif

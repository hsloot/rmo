#ifndef RMO_SETS_H_
#define RMO_SETS_H_

inline bool is_within(const unsigned int &i, const unsigned int &j)  {
  return (j >> (i-1)) % 2 == 1;
} // inline bool is_within(const unsigned int &i, const unsigned int &j);

#endif

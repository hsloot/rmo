#ifndef MO_UTILS_IMPLEMENTATION_REVSORT_IPP
#define MO_UTILS_IMPLEMENTATION_REVSORT_IPP

#include <vector>

namespace mo {
namespace utils {

// adapted from https://github.com/wch/r-source/blob/trunk/src/main/sort.c
template<typename Vector, typename IndexVector>
inline void reverse_sort(Vector& a, IndexVector& ib) {
  /*
  *  R : A Computer Language for Statistical Data Analysis
  *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
  *  Copyright (C) 1998-2018   The R Core Team
  *  Copyright (C) 2004        The R Foundation
  *
  *  This program is free software; you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation; either version 2 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program; if not, a copy is available at
  *  https://www.R-project.org/Licenses/
  */

  if (a.size() <= 1) return;
  auto n = a.size();
  auto l = n >> 1;
  auto ir = n-1;
  auto ra = a[0];
  auto ii = ib[0];

  for (std::size_t i, j; ; ) {
    if (l > 0) {
      l=l-1;
      ra = a[l];
      ii = ib[l];
    }
    else {
      ra = a[ir];
      ii = ib[ir];
      a[ir] = a[0];
      ib[ir] = ib[0];
      if (--ir == 0) {
        a[0] = ra;
        ib[0] = ii;
        return;
      }
    }
    i = l;
    j = ((l+1) << 1)-1;
    while (j <= ir) {
      if (j < ir && a[j] > a[j + 1]) ++j;
      if (ra > a[j]) {
        a[i] = a[j];
        ib[i] = ib[j];
        i = j;
        j = ((i+1) << 1)-1;
        // j += (i = j)+1;
      }
      else
        j = ir + 1;
    }
    a[i] = ra;
    ib[i] = ii;
  }
}

} // utils
} // mo

#endif // MO_UTILS_IMPLEMENTATION_REVSORT_IPP

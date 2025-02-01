#include <xrates_dp.hpp>
#include <algorithm>
#include <cmath>
#include <cassert>
#include <iostream>

#include <boost/format.hpp>
using namespace std;
using fmt = boost::format;
size_t count_1 = 0;
size_t count_2 = 0;

namespace xrate {

  vector<string_view> build_currency_list(const Matrix& rates) {
    auto currencies = vector<string_view>(rates.size());
    transform(rates.begin(), rates.end(), currencies.begin(),
      [](auto& kv) { return kv.first; } );
    return currencies;
  }

  CurCell combine(CurCell lhs, CurCell rhs) {
    return {lhs.first*rhs.first, lhs.second+rhs.second};
  }

  CurCell backward(CurCell lhs) {
    //return {isnan(lhs.first) ? NAN : (1/lhs.first), lhs.second+1};
    return {1/lhs.first, lhs.second+1};
  }

  bool isbetter(CurCell current, CurCell candidate) {
    return !isnan(candidate.first) 
           && (isnan(current.first) || candidate.second < current.second);
  }

  bool isbest(CurCell lhs) {
    return (!isnan(lhs.first) && lhs.second <= 3);
  }

  auto build_cell_it(const vector<string_view>& currencies) {
    auto it = [
        &currencies, 
        c1_it = size_t(0),
        c2_it = size_t(0)]() mutable {
      const auto len = currencies.size();
      for(; c1_it < len; ++c1_it, c2_it=0)
        for(; c2_it < len;) {
          auto& c1 = currencies[c1_it];
          auto& c2 = currencies[c2_it++];
          if (c1 != c2)
            return make_pair(c1,c2);
        }
      return CurPair("", "");
    };
    return it;
  }

  void patch_backpaths(Matrix& rates, const vector<string_view>& currencies) {
    auto iterator_over_cells = build_cell_it(currencies);

    for(auto pcurr = iterator_over_cells();
        pcurr.first.size();
        pcurr = iterator_over_cells()) {
      auto from = pcurr.first;
      auto to = pcurr.second;
      auto irate = backward(rates[to][from]);
      auto& rate = rates[from][to];
      if(isbetter(rate, irate)) rate = irate;
    }
  }

  auto roll_span(span<string_view>& currencies) {
    auto it = [
        &currencies, 
        idx = size_t(0),
        next = size_t(0)
        ]() mutable {
      swap(currencies[0], currencies[idx]);
      idx = next;
      if (idx < currencies.len) swap(currencies[0], currencies[idx]);
      next = ++next;
      return make_pair(idx, currencies.subspan(1));
    };
    return it;
  }

  void roll_restore_order(span<std::string_view>& span, size_t idx) {
    if (idx < span.len) swap(span[0], span[idx]);
  }

  CurCell find_shortest_helper(Matrix& rates, span<string_view>& rolling_span, string_view from, string_view to) {
    count_1++;
    auto& final_rate = rates[from][to];

    if (from != to && !isbest(final_rate) && rolling_span.len) {
      auto roll_it = roll_span(rolling_span);
      auto& inverse = rates[to][from];

      for(auto idx_span = roll_it();
          idx_span.first < rolling_span.len;
          idx_span = roll_it() ) {
        count_2++;
        auto cur = rolling_span[0];
        auto rate = combine(
          find_shortest_helper(rates, idx_span.second, from, cur),
          find_shortest_helper(rates, idx_span.second, cur, to));
        auto irate = backward(rate);
        if(isbetter(final_rate, rate))
          final_rate = rate;
        if(isbetter(inverse, irate))
          inverse = irate;
      }
    }
    return final_rate;
  }

  void find_shortest(Matrix& rates, vector<string_view> currencies, string_view from, string_view to) {
    assert(from != to);
    auto& final_rate = rates[from][to];
    auto& inverse = rates[to][from];

    auto rolling_span = span<string_view>::from_vector(currencies);
    auto roll_it = roll_span(rolling_span);

    for(auto idx_span = roll_it();
        idx_span.first < rolling_span.len;
        idx_span = roll_it() ) {
      auto cur = rolling_span[0];
      auto rate = combine(
        find_shortest_helper(rates, idx_span.second, from, cur),
        find_shortest_helper(rates, idx_span.second, cur, to));
      auto irate = backward(rate);
      if(isbetter(final_rate, rate))
        final_rate = rate;
      if(isbetter(inverse, irate))
        inverse = irate;
    }
  }

  void all_pairs_shortest(Matrix& rates) {
    auto currencies = build_currency_list(rates);
    auto iterator_over_cells = build_cell_it(currencies);

    patch_backpaths(rates, currencies);
    print_exchange_matrix(rates);

    for(auto pcurr = iterator_over_cells();
        pcurr.first.size();
        pcurr = iterator_over_cells()) {
      auto from = pcurr.first;
      auto to = pcurr.second;
      find_shortest(rates, currencies, from, to);
    }
  }

  void print_exchange_matrix(const Matrix& rates) {
    auto currencies = build_currency_list(rates);
    sort(currencies.begin(), currencies.end());

    cout << "      ";
    for(auto from : currencies) {
      cout << from << "    ";
    }
    cout << endl;
    //for(auto& [from,row] : rates) {
    for(auto from : currencies) {
      cout << from << "   ";
      auto& row = rates.at(from);
      //for(auto [to,cell] : row) {
      for(auto to : currencies) {
        auto cell = row.at(to);
        if(isnan(cell.first)) cell.first = 0.0;
        //cout << fmt("%s:%.2f") % to % cell << " ";
        cout << fmt("%.2f,%d") % cell.first % cell.second << " ";
      }
      cout << endl;
    }
    cout << endl;
  }

};

int main(int argc, char**argv) {
  vector<xrate::Matrix> rates_sq = {
    { { "USD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {3.5,1}} } },
      { "EUR", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {3.2,1}},{"HKD", {NAN,1}} } },
      { "CHF", { {"USD", {NAN,1}},{"EUR", {0.8,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "TWD", { {"USD", {0.3,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "HKD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },},
    { { "USD", { {"USD", {NAN,1}},{"EUR", {0.8,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "EUR", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {1.2,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "CHF", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {4.0,1}},{"HKD", {NAN,1}} } },
      { "TWD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {2.0,1}} } },
      { "HKD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },},
    { { "USD", { {"USD", {NAN,1}},{"EUR", {0.8,1}},{"CHF", {1.0,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "EUR", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {5.2,1}},{"HKD", {NAN,1}} } },
      { "CHF", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {4.5,1}} } },
      { "TWD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },
      { "HKD", { {"USD", {NAN,1}},{"EUR", {NAN,1}},{"CHF", {NAN,1}},{"TWD", {NAN,1}},{"HKD", {NAN,1}} } },},
  };

  for(auto& rates : rates_sq) {
    xrate::print_exchange_matrix(rates);
    xrate::all_pairs_shortest(rates);
    xrate::print_exchange_matrix(rates);
    cout << fmt("count_1=%d, count_2=%d") % count_1 % count_2 << endl;
  }
  return 0;
}


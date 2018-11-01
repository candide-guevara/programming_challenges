#include <xrates_dp.hpp>
#include <algorithm>
#include <cmath>
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

  auto build_missing_it(const Matrix& rates, const vector<string_view>& currencies) {
    auto it = [
        &currencies, 
        &rates,
        c1_it = size_t(0),
        c2_it = size_t(0)]() mutable {
      const auto len = currencies.size();
      for(; c1_it < len; ++c1_it, c2_it=0)
        for(; c2_it < len;) {
          auto& c1 = currencies[c1_it];
          auto& c2 = currencies[c2_it++];
          if (c1 != c2 && isnan(rates.at(c1).at(c2)))
            return make_pair(c1,c2);
        }
      return CurPair("", "");
    };
    return it;
  }

  void patch_backpaths(Matrix& rates, const vector<string_view>& currencies) {
    auto iterator_over_missing = build_missing_it(rates, currencies);

    for(auto pcurr = iterator_over_missing();
        pcurr.first.size();
        pcurr = iterator_over_missing()) {
      auto from = pcurr.first;
      auto to = pcurr.second;
      auto inverse = rates[to][from];
      //if (!isnan(inverse))
        rates[from][to] = 1 / inverse;
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

  double find_shortest_helper(Matrix& rates, span<string_view>& rolling_span, string_view from, string_view to) {
    count_1++;
    auto& final_rate = rates[from][to];

    if (from != to && isnan(final_rate) && rolling_span.len) {
      count_2++;
      auto roll_it = roll_span(rolling_span);
      auto& inverse = rates[to][from];

      for(auto idx_span = roll_it();
          idx_span.first < rolling_span.len;
          idx_span = roll_it() ) {
        auto cur = rolling_span[idx_span.first];
        auto rate = find_shortest_helper(rates, idx_span.second, from, cur)
                    * find_shortest_helper(rates, idx_span.second, cur, to);
        if(!isnan(rate))
          final_rate = rate;
        if(isnan(inverse))
          inverse = 1/rate;
      }
    }
    return final_rate;
  }

  void find_shortest(Matrix& rates, vector<string_view> currencies, string_view from, string_view to) {
    auto rolling_span = span<string_view>::from_vector(currencies);
    auto roll_it = roll_span(rolling_span);

    for(auto idx_span = roll_it();
        idx_span.first < rolling_span.len;
        idx_span = roll_it() ) {
      auto cur = rolling_span[idx_span.first];
      auto rate = find_shortest_helper(rates, idx_span.second, from, cur)
                  * find_shortest_helper(rates, idx_span.second, cur, to);
      auto& inverse = rates[to][from];
      if(!isnan(rate))
        rates[from][to] = rate;
      if(isnan(inverse))
        inverse = 1/rate;
    }
  }

  void all_pairs_shortest(Matrix& rates) {
    auto currencies = build_currency_list(rates);
    auto iterator_over_missing = build_missing_it(rates, currencies);

    patch_backpaths(rates, currencies);
    //print_exchange_matrix(rates);

    for(auto pcurr = iterator_over_missing();
        pcurr.first.size();
        pcurr = iterator_over_missing()) {
      auto from = pcurr.first;
      auto to = pcurr.second;
      find_shortest(rates, currencies, from, to);
    }
    return;
  }

  void print_exchange_matrix(const Matrix& rates) {
    auto currencies = build_currency_list(rates);
    sort(currencies.begin(), currencies.end());

    cout << "      ";
    for(auto from : currencies) {
      cout << from << "  ";
    }
    cout << endl;
    //for(auto& [from,row] : rates) {
    for(auto from : currencies) {
      cout << from << "   ";
      auto& row = rates.at(from);
      //for(auto [to,cell] : row) {
      for(auto to : currencies) {
        auto cell = row.at(to);
        if(isnan(cell)) cell = 0.0;
        //cout << fmt("%s:%.2f") % to % cell << " ";
        cout << fmt("%.2f") % cell << " ";
      }
      cout << endl;
    }
    cout << endl;
  }

};

int main(int argc, char**argv) {
  vector<xrate::Matrix> rates_sq = {
    {{ "USD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", 3.5} } },
    { "EUR", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", 3.2},{"HKD", NAN} } },
    { "CHF", { {"USD", NAN},{"EUR", 0.8},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },
    { "TWD", { {"USD", 0.3},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },
    { "HKD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },},
    {{ "USD", { {"USD", NAN},{"EUR", 0.8},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },
    { "EUR", { {"USD", NAN},{"EUR", NAN},{"CHF", 1.2},{"TWD", NAN},{"HKD", NAN} } },
    { "CHF", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", 4.0},{"HKD", NAN} } },
    { "TWD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", 2.0} } },
    { "HKD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },},
    {{ "USD", { {"USD", NAN},{"EUR", 0.8},{"CHF", 1.0},{"TWD", NAN},{"HKD", NAN} } },
    { "EUR", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", 5.2},{"HKD", NAN} } },
    { "CHF", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", 4.5} } },
    { "TWD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },
    { "HKD", { {"USD", NAN},{"EUR", NAN},{"CHF", NAN},{"TWD", NAN},{"HKD", NAN} } },},
  };

  for(auto& rates : rates_sq) {
    xrate::print_exchange_matrix(rates);
    xrate::all_pairs_shortest(rates);
    xrate::print_exchange_matrix(rates);
    cout << fmt("count_1=%d, count_2=%d") % count_1 % count_2 << endl;
  }
  return 0;
}


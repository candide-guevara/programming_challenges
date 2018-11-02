#pragma once
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace xrate {
  using CurCell = std::pair<double, int>;
  using Matrix = std::unordered_map<std::string_view, std::unordered_map<std::string_view, CurCell>>;
  using CurPair = std::pair<std::string_view, std::string_view>;

  template<class T>
  struct span {
    T* start;
    size_t len;
    static span from_vector(std::vector<T>& vec) { return {vec.data(), vec.size()}; }
    span subspan(size_t mv) { return {start+mv, len-mv}; }
    T& operator[](size_t i) { return start[i]; }
    T* begin() { return start; }
    T* end() { return start+len; }
  };

  std::vector<std::string_view> build_currency_list(const Matrix& rates);
  CurCell combine(CurCell lhs, CurCell rhs);
  CurCell backward(CurCell lhs);
  bool isbest(CurCell lhs);
  bool isbetter(CurCell current, CurCell candidate);
  void patch_backpaths(Matrix& rates, const std::vector<std::string_view>& currencies);
  auto roll_span(span<std::string_view>& currencies);
  void roll_restore_order(span<std::string_view>& span, size_t idx);
  auto build_cell_it(const Matrix& rates, const std::vector<std::string_view>& currencies);
  void find_shortest(Matrix& rates, std::vector<std::string_view> currencies, std::string_view from, std::string_view to);
  double find_shortest_helper(Matrix& rates, const span<std::string_view>& rolling_span, std::string_view from, std::string_view to);
  void all_pairs_shortest(Matrix& rates);
  void print_exchange_matrix(const Matrix& rates);
};


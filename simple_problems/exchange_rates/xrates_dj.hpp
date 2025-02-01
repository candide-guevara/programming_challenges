#pragma once
#include <queue>
#include <string>
#include <vector>
#include <tuple>
#include <unordered_map>

namespace xrate {

  struct Currency;

  struct XRate {
    const Currency *from, *to;
    double value;
    int weight;

    std::string to_string() const;
  };

  struct Currency {
    std::string name;
    std::vector<XRate> rates;

    std::string to_string() const;
  };

  struct TravItem {
    const Currency* parent;
    double value;
    int weight;
  };

  using RateItems = std::vector<std::tuple<std::string, std::string, double>>;
  using ShortRates = std::vector<XRate>;
  using AllPairs = std::vector<std::pair<const Currency*, ShortRates>>;

  struct RateGraph {
    std::vector<Currency> nodes;

    RateGraph(const RateItems& items);
    const Currency& get_by_name(std::string cur) const;
    ShortRates get_shortest_from(std::string from) const;
    AllPairs get_all_pairs_short() const;
    std::string to_string() const;
  };

  struct TravState {
    std::vector<TravItem> nodes;
    std::vector<TravItem*> prio_q;
    std::vector<TravItem*>::iterator prio_head;
    std::unordered_map<const Currency*, TravItem*> lookup;

    TravState(const RateGraph& graph, std::string start_cur);
    const TravItem* const peek() const;
    TravItem* const pop();
    size_t left_to_visit() const;
    void reorder_q();
  };

  void print_rates(const ShortRates& rates);
  void print_rates(const AllPairs& rates);
}


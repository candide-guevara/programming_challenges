#include <xrates_dj.hpp>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <limits>
#include <sstream>
#include <string_view>

#include <boost/format.hpp>

using namespace std;
using fmt = boost::format;

namespace xrate {

  RateGraph::RateGraph(const RateItems& items) {
    nodes.reserve(2 * items.size());
    unordered_map<string_view, Currency*> lookup;

    for(auto& [from,to,value] : items) {
      auto& from_ptr = lookup[from];
      auto& to_ptr = lookup[to];

      if (!from_ptr) {
        auto& cur = nodes.emplace_back();
        cur.name = from;
        from_ptr = &cur;
      }
      if (!to_ptr) {
        auto& cur = nodes.emplace_back();
        cur.name = to;
        to_ptr = &cur;
      }

      from_ptr->rates.emplace_back(XRate{from_ptr, to_ptr, value, 1});
      to_ptr->rates.emplace_back(XRate{to_ptr, from_ptr, 1/value, 2});
    }
  }

  const Currency& RateGraph::get_by_name(std::string name) const {
    auto it = find_if(nodes.begin(), nodes.end(),
                      [&name](auto& cur) { return name == cur.name; } );
    assert(it != nodes.end());
    return *it;
  }

  ShortRates RateGraph::get_shortest_from(std::string from) const {
    auto state = TravState(*this, from);
    auto start_ptr = state.peek()->parent;
    auto result = ShortRates();
    result.reserve(nodes.size());

    while(state.left_to_visit()) {
      auto item_ptr = state.pop();
      result.emplace_back(XRate{start_ptr, item_ptr->parent, item_ptr->value, item_ptr->weight});

      for(auto& edge : item_ptr->parent->rates) {
        auto tgt_ptr = state.lookup[edge.to];
        assert(tgt_ptr);
        tgt_ptr->weight = item_ptr->weight + edge.weight;
        tgt_ptr->value = item_ptr->value * edge.value;
        assert(!isnan(tgt_ptr->value) && tgt_ptr->weight >= 0);
      }
      state.reorder_q();
    }
    return result;
  }

  AllPairs RateGraph::get_all_pairs_short() const {
    auto all_pairs = AllPairs{};
    for(auto& n : nodes) {
      auto shortest = get_shortest_from(n.name);
      all_pairs.emplace_back(&n, move(shortest));
    }
    return all_pairs;
  }


  TravState::TravState(const RateGraph& graph, std::string start_cur) {
    nodes.reserve(graph.nodes.size());
    for(auto& cur : graph.nodes) {
      auto& n = nodes.emplace_back(TravItem{&cur, NAN, numeric_limits<int>::max()});
      if(start_cur == cur.name) {
        n.value = 1.0;
        n.weight = 0;
      }
      lookup[&cur] = &n;
      prio_q.push_back(&n);
    }
    prio_head = prio_q.begin();
    reorder_q();
  }

  const TravItem* const TravState::peek() const {
    return *prio_head;
  }

  TravItem* const TravState::pop() {
    auto head = *prio_head;
    ++prio_head;
    return head;
  }

  size_t TravState::left_to_visit() const {
    auto chead = std::vector<TravItem*>::const_iterator(prio_head);
    return distance(chead, prio_q.end());
  }

  void TravState::reorder_q() {
    auto comp = [](auto lhs, auto rhs) {
      return (lhs->weight > rhs->weight)
             || (lhs->weight == rhs->weight && lhs->parent > rhs->parent);
    };
    make_heap(prio_head, prio_q.end(), comp);
  }


  std::string XRate::to_string() const {
    return (fmt("%s->%s:%.2f/%d") % from->name % to->name % value % weight).str();
  }

  std::string Currency::to_string() const {
    auto ss = stringstream{};
    ss << name << "  ";
    for(auto& r : rates)
      ss << fmt("%s:%.2f/%d") % r.to->name % r.value % r.weight << ", ";
    ss << endl;
    return ss.str();
  }

  std::string RateGraph::to_string() const {
    auto ss = stringstream{};
    for(auto& n : nodes)
      ss << n.to_string();
    return ss.str();
  }

  void print_rates(const ShortRates& rates) {
    for(auto& r : rates)
      cout << r.to_string() << ", ";
    cout << endl;
  }

  void print_rates(const AllPairs& rates) {
    for(auto& [cur, shortest] : rates) {
      print_rates(shortest);
    }
  }

}

int main(int argc, char**argv) {
  vector<xrate::RateItems> rates_sq = {
    { {"USD", "HKD", 3.5},
      {"EUR", "TWD", 3.2},
      {"CHF", "EUR", 0.8},
      {"TWD", "USD", 0.3} },
    { {"USD", "EUR", 0.8},
      {"EUR", "CHF", 1.2},
      {"CHF", "TWD", 4.0},
      {"TWD", "HKD", 2.0} },
    { {"USD", "EUR", 0.8},
      {"USD", "CHF", 1.0},
      {"EUR", "TWD", 5.2},
      {"CHF", "HKD", 4.5} },
  };

  for(auto& rates : rates_sq) {
    auto graph = xrate::RateGraph(rates);
    cout << graph.to_string();
    auto result = graph.get_all_pairs_short();
    xrate::print_rates(result);
  }
  return 0;
}


#include <algorithm>
#include <array>
#include <format>
#include <iostream>
#include <string>
#include <vector>

namespace {
constexpr int NUM_BOXES = 256;

struct Lens {
  std::string label;
  int focal_length;
};

std::vector<Lens>::iterator find_lens(std::vector<Lens>& box,
                                      std::string_view label) {
  return std::find_if(box.begin(), box.end(), [label](const Lens& lens) {
    return lens.label == label;
  });
}

void add_lens(std::vector<Lens>& box, std::string_view label,
              int focal_length) {
  auto match = find_lens(box, label);
  if (match != box.end()) {
    match->focal_length = focal_length;
  } else {
    box.push_back(Lens{std::string(label), focal_length});
  }
}

void remove_lens(std::vector<Lens>& box, std::string_view label) {
  auto match = find_lens(box, label);
  if (match != box.end()) {
    box.erase(match);
  }
}

}  // namespace

int main() {
  // if you're using a "HashMap" to represent the boxes then you probably
  // don't know what a HashMap is doing. (note that I said "probably").
  // Calculating the box number _is_ hashing. You're already halfway there!
  std::array<std::vector<Lens>, NUM_BOXES> boxes;
  std::string term{};
  while (std::getline(std::cin, term, ',')) {
    int box_id{};
    for (auto it = term.begin(), end = term.end() - (term.back() == '\n');
         it != end; ++it) {
      if (*it == '-') {
        auto label = std::string_view(term.begin(), it);
        remove_lens(boxes[box_id], label);
      } else if (*it == '=') {
        auto label = std::string_view(term.begin(), it);
        auto focal_length = std::stoi(std::string(it + 1, end));
        add_lens(boxes[box_id], label, focal_length);
        break;
      } else {
        box_id = ((box_id + *it) * 17) % NUM_BOXES;
      }
    }
  }
  long total = 0;
  int box_num = 1;
  for (const auto& box : boxes) {
    int slot_num = 1;
    for (const auto& lens : box) {
      total += box_num * slot_num * lens.focal_length;
      ++slot_num;
    }
    ++box_num;
  }
  std::cout << total << '\n';
}

package day_3_java;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

class P2 {
  public static void check(boolean v) {
    if (!v) {
      throw new RuntimeException("Assertion failed");
    }
  }
  public static void main(String[] args) {
    // Read file into string
    String rawInput;
    try {
      rawInput = Files.readString(Paths.get("day_3_java/input.txt"));
    } catch (Exception e) {
      System.out.printf("Error reading file %s", e.toString());
      return;
    }

    List<String> lines = Arrays.asList(rawInput.split("\\r?\\n"));

    // Make sure each input is an even length
    lines.stream()
      .map(s -> (s.length()%2) == 0)
      .forEach(Part1::check);
    
    List<Set<Character>> rucksacks = lines.stream()
      .map(s -> s.chars()
        .mapToObj(c -> (char)c)
        .collect(Collectors.toSet())
      ).collect(Collectors.toList());

    // make sure that the number of rucksacks is a multiple of 3
    check(rucksacks.size()%3 == 0);
    
    // group each 3 rucksacks into a list
    List<List<Set<Character>>> rucksackGroups = rucksacks.stream()
      .collect(Collectors.groupingBy(s -> rucksacks.indexOf(s)/3))
      .values().stream()
      .collect(Collectors.toList());
    
      List<Character> rucksacksBadge = rucksackGroups.stream()
       .map(g -> g.stream()
          .reduce((a, b) -> {
            a.retainAll(b);
            return a;
          }).get().stream().findFirst().get()
          ).collect(Collectors.toList());

      List<Integer> rucksacksPriority = rucksacksBadge.stream()
        .map(c -> {
          if (Character.isUpperCase(c)) {
            return (int)c - (int)'A' + 27;
          } else {
            return (int)c - (int)'a' + 1;
          }
        }).collect(Collectors.toList());
      rucksacksPriority.stream().reduce((cur, acc) -> cur + acc).ifPresent(System.out::println);
  }
}
package day_3_java;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

class Part1 {
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
    
    List<List<String>> rucksacks = lines.stream()
      .map(s -> Arrays.asList(
        s.substring(0, s.length()/2),
        s.substring(s.length()/2)
      )).collect(Collectors.toList());
    
    List<List<Set<Character>>> rucksacksSets = rucksacks.stream()
      .map(compartments -> compartments.stream()
        .map(compartment -> compartment.chars()
          .mapToObj(c -> (char)c)
          .collect(Collectors.toSet())
        ).collect(Collectors.toList())
      ).collect(Collectors.toList());

    List<Set<Character>> rucksackShared = rucksacksSets.stream()
      .map(compartments -> compartments.stream()
        .reduce((a, b) -> {
          a.retainAll(b);
          return a;
        }).get()
      ).collect(Collectors.toList());
    
    // Make sure each is length 1
    rucksackShared.stream().map(s -> s.size() == 1).forEach(Part1::check);

    List<Integer> rucksacksPriority = rucksackShared.stream()
      .map(s -> s.stream().findFirst().get())
      .map(c -> {
        if (Character.isUpperCase(c)) {
          return (int)c - (int)'A' + 27;
        } else {
          return (int)c - (int)'a' + 1;
        }
      })
      .collect(Collectors.toList());
    rucksacksPriority.stream().reduce((cur, acc) -> cur + acc).ifPresent(System.out::println);
  }
}
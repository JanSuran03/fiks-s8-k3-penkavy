package penkavy;

import java.util.*;


class LevenshteinDistanceRecursive {

    public static int compute_Levenshtein_distance(String str1,
                                                   String str2) {

        if (str1.isEmpty()) {
            return str2.length();
        }


        if (str2.isEmpty()) {
            return str1.length();
        }

        int replace = compute_Levenshtein_distance(
                str1.substring(1), str2.substring(1))
                + NumOfReplacement(str1.charAt(0), str2.charAt(0));

        int insert = compute_Levenshtein_distance(
                str1, str2.substring(1)) + 1;

        int delete = compute_Levenshtein_distance(
                str1.substring(1), str2) + 1;

        return minm_edits(replace, insert, delete);
    }

    public static int NumOfReplacement(char c1, char c2) {

        return c1 == c2 ? 0 : 1;
    }

    public static int minm_edits(int... nums) {

        return Arrays.stream(nums).min().orElse(
                Integer.MAX_VALUE);
    }
}
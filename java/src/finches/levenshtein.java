package finches;

public class Levenshtein {
    Levenshtein() {
    }

    static public int[][] init_2d_array(int height, int width) {
        int[][] ret_arr = new int[height][width];
        for (int i = 0; i < height; i++) {
            int[] in_arr = new int[width];
            for (int j = 0; j < width; j++) {
                in_arr[j] = 0;
            }
            ret_arr[i] = in_arr;
        }
        return ret_arr;
    }

    static public boolean compute_levenshtein(String str1, String str2, Long max_diff) {
        int len_1 = str1.length();
        int len_2 = str2.length();
        int[][] array = init_2d_array(str1.length(), str2.length());
        // init first row and column
        for (int i = 0; i < len_1; i++) {
            array[i][0] = i;
        }
        for (int i = 0; i < len_2; i++) {
            array[0][i] = i;
        }

        for (int i = 1; i < len_1; i++) {
            for (int j = 1; j < len_2; j++) {
                int dec_i = i - 1;
                int dec_j = j - 1;
                char a_dec_i = str1.charAt(dec_i);
                char a_dec_j = str2.charAt(dec_j);
                if (a_dec_i == a_dec_j) {
                    array[i][j] = array[dec_i][dec_j];
                } else {
                    int insertion = array[i][dec_j] + 1;
                    int deletion = array[dec_i][j] + 1;
                    int replacement = array[dec_i][dec_j] + 1;
                    array[i][j] = Math.min(Math.min(insertion, deletion), replacement);
                }
            }
        }
        return array[len_1 - 1][len_2 - 1] <= max_diff;
    }
}

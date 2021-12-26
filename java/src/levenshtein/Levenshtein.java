package levenshtein;

public class Levenshtein {
    Levenshtein() {
    }

    static public int[][] init_2d_array(int width) {
        int[][] ret_arr = new int[2][width];
        for (int i = 0; i < 2; i++) {
            int[] in_arr = new int[width];
            for (int j = 0; j < width; j++) {
                in_arr[j] = 0;
            }
            ret_arr[i] = in_arr;
        }
        return ret_arr;
    }

    static public boolean compute_levenshtein(String str1, String str2, Long max_diff) {
        int len_1 = str1.length() + 1;
        int len_2 = str2.length() + 1;
        int[][] arr = init_2d_array(len_2);
        // init first row
        for (int i = 0; i < len_2; i++) {
            arr[0][i] = i;
        }
        for (int i = 1; i < len_1; i++) {
            int i_half = i % 2;
            arr[i_half][0] = i;
            for (int j = 1; j < len_2; j++) {
                int dec_i_half = (i - 1) % 2;
                int dec_j = j - 1;
                if (str1.charAt(i - 1) == str2.charAt(dec_j)) {
                    arr[i_half][j] = arr[dec_i_half][dec_j];
                } else {
                    int insertion = arr[i_half][dec_j];
                    int deletion = arr[dec_i_half][j];
                    int replacement = arr[dec_i_half][dec_j];
                    arr[i_half][j] = Math.min(Math.min(insertion, deletion), replacement) + 1;
                }
            }
        }
        return arr[(len_1 - 1) % 2][len_2 - 1] <= max_diff;
    }
}
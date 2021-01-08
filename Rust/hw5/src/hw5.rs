
/// P1
/// Minimum valid parentheses string
///
/// Given a string of characters and parentheses, remove the minimum
/// number of parentheses to make the parentheses balanced within the
/// string. Note that the result may be an empty string.
///
/// For example, given
///     given: "(hel(l))o)"
///     return: "(hel(l))o" or "(hel(l)o)"
///
/// and
///     given: "))(("
///     return: ""
///
#[allow(dead_code)]
pub fn minimum_balanced_string(s: &str) -> String {
    let mut a = String::from("");
    let mut balance = 0;
    for x in s.chars(){
        if x == '(' {
            balance += 1;
            a.push(x);
        }
        else if x == ')' {
            if balance > 0 {
                a.push(x);
                balance -= 1;
            }
        } else {
            a.push(x);
        }
    }
    let mut b = String::from("");
    balance = 0;
    for x in a.chars().rev(){
        if x == ')' {
            balance += 1;
            b.push(x);
        }
        else if x == '(' {
            if balance > 0 {
                b.push(x);
                balance -= 1;
            }
        }
        else {
            b.push(x);
        }
    }
    b = b.chars().rev().collect();
    b.to_string()
}

/// P2
/// Find all anagrams
///
/// Given a base string and a pattern string, return a vector containing
/// the start indices in the base string that contain substrings that are
/// anagrams of the pattern string.
///
/// Given a base string of "abab" and a pattern string of "ab" return
/// the vector [0, 1, 2] since "ab" is an anagram of "ab" (at index 0)
/// "ba" (at index 1) and "ab" (at index 2).
///
/// Given a base string of "abba" and a pattern string of "ba" return
/// the vector [0, 2] since "ba" is an anagram of "ab" (at index 0) and
/// an anagram of "ba" (at index 2)
#[allow(dead_code)]
pub fn all_anagrams(base: &str, pattern: &str) -> Vec<usize> {

    let mut x = [0; 26];
    let mut result = vec![];

    for c in pattern.chars() {
        //print!("{}, {} ", c, c as usize);
        x[(c as usize) - ('a' as usize)] += 1;
    }

    let pattern_len = pattern.len();
    let s:Vec<char> = base.chars().collect();
    for i in 0..s.len() {

        //println!("{}", s[i] as usize);
        //println!("{}", 'a' as usize);
        x[(s[i] as usize) - ('a' as usize)] -= 1;

        if i >= pattern_len {
            x[(s[i - pattern_len] as usize) - ('a' as usize)] += 1;
        }

        if i >= pattern_len - 1 && anagram_match(&x) {
            result.push(i+1 - pattern_len);
        }
    }

    fn anagram_match(x: &[i32]) -> bool {

        let mut ans = true;
        for i in x {
            if *i != 0 {
                ans = false;
                break;
            }
        }
        ans
    }

    result
}


/// P3
/// Merge two sorted lists
///
/// Given two sorted lists, return a new list that contains the elements of both
/// input lists, in sorted orderd. Do this in O(n) time (in other words, don't
/// combine the lists and then call the library sort function).
///
/// Example: given [1, 3, 5] and [2, 4, 6], return [1, 2, 3, 4, 5, 6]
#[allow(dead_code)]
pub fn merge_sorted_lists<T: Ord + Copy>(l1: Vec<T>, l2: Vec<T>) -> Vec<T> {
    let mut sorted_list = vec![];
    let mut left = 0;
    let mut right = 0;

    while left < l1.len() && right < l2.len() {
        if l1[left] <= l2[right] {
            sorted_list.push(l1[left]);
            left += 1;
        }
        else {
            sorted_list.push(l2[right]);
            right += 1;
        }
    }

    if left < l1.len(){
        sorted_list.extend_from_slice(&l1[left..]);
    }
    if right < l2.len(){
        sorted_list.extend_from_slice(&l2[right..]);
    }
    /*sorted_list.extend(l1);
    sorted_list.extend(l2);
    for x in 0..sorted_list.len(){
        for y in 0..sorted_list.len() - x - 1 {
            if sorted_list[y+1] < sorted_list[y]{
                sorted_list.swap(y, y+1);
            }
        }
    }*/
    sorted_list
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_balanced_string_1() {
        assert_eq!(minimum_balanced_string("))(("), "");
    }

    #[test]
    fn min_balanced_string_2() {
        assert_eq!(minimum_balanced_string("(hel(lo)))"), "(hel(lo))");
    }

    #[test]
    fn min_balanced_string_3() {
        assert_eq!(minimum_balanced_string("(())))"), "(())");
    }

    #[test]
    fn min_balanced_string_4() {
        assert_eq!(minimum_balanced_string("(bira)((s(abc)"), "(bira)s(abc)");
    }

    #[test]
    fn all_anagrams_1() {
        assert_eq!(all_anagrams("abab", "ab"), vec![0, 1, 2]);
    }

    #[test]
    fn all_anagrams_2() {
        assert_eq!(all_anagrams("abba", "ba"), vec![0, 2]);
    }

    #[test]
    fn all_anagrams_3() {
        assert_eq!(all_anagrams("", ""), vec![]);
    }

    #[test]
    fn merge_sorted_1() {
        assert_eq!(
            merge_sorted_lists(vec![1, 3, 5], vec![2, 4, 6]),
            vec![1, 2, 3, 4, 5, 6]
        );
    }

    #[test]
    fn merge_sorted_2() {
        assert_eq!(
            merge_sorted_lists(vec!["a", "b", "c"], vec!["d", "e", "f"]),
            vec!["a", "b", "c", "d", "e", "f"]
        );
    }

    #[test]
    fn merge_sorted_3() {
        assert_eq!(
            merge_sorted_lists(vec![-5,-3,0,2,3], vec![-2,-1,1,5,8]),
            vec![-5, -3, -2, -1, 0, 1, 2, 3, 5, 8]
        );
    }
}

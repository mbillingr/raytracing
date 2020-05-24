pub fn partition<T: Copy + PartialOrd>(n: usize, data: &mut [T]) {
    partition_by_key(n, data, |&x| x)
}

pub fn sweep<T: Copy + PartialOrd>(pivot: usize, data: &mut [T]) -> usize {
    sweep_by_key(pivot, data, |&x| x)
}

/// Partition data inplace so that the nth smallest value is at position n,
/// all values to it's left are smaller or equal, and all values to it's right
/// are larger or equal.
pub fn partition_by_key<T, K: PartialOrd, F: Copy + Fn(&T) -> K>(n: usize, data: &mut [T], key: F) {
    //data.sort_unstable_by(|a, b| key(a).partial_cmp(&key(b)).unwrap())
    if n == 0 && data.len() == 1 {
        return;
    }

    let pivot = data.len() / 2;
    let pivot = sweep_by_key(pivot, data, key);

    if n < pivot {
        partition_by_key(n, &mut data[..pivot], key);
    } else if n > pivot {
        partition_by_key(n - pivot - 1, &mut data[pivot + 1..], key)
    }
}

/// Rearrange data so that all values on the left are <= and all values to the
/// right are >= compared to the value at given pivot index, with the pivot
/// value in-between. The pivot may need to be moved; it's new index is returned.
///
///    Given:
///        p0, data, and v0 = data[p0]
///    When:
//         p = sweep(pivot, data)
///    Then:
///        data[i] <= v0 if i < p
///        data[p] == v0
///        data[j] >= v0 if j > p
pub fn sweep_by_key<T, K: PartialOrd, F: Fn(&T) -> K>(
    mut pivot: usize,
    data: &mut [T],
    key: F,
) -> usize {
    let pivot_value = key(&data[pivot]);

    let mut left = 0;
    let mut right = data.len() - 1;

    loop {
        while key(&data[left]) < pivot_value {
            left += 1;
        }
        while key(&data[right]) > pivot_value {
            right -= 1;
        }
        if left >= right {
            break;
        }

        if left == pivot {
            pivot = right;
        } else if right == pivot {
            pivot = left;
        }

        data.swap(left, right);
        left += 1;
        right -= 1;
    }

    if left == right {
        // if left and right stop at the same cell, this must be a valid pivot
        left
    } else {
        if pivot > left {
            data.swap(left, pivot);
            left
        } else if pivot < right {
            data.swap(right, pivot);
            right
        } else {
            pivot
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn check_partition(val: i32, pivot: usize, data: &[i32]) {
        for &i in &data[..pivot] {
            assert!(i <= val);
        }
        for &k in &data[pivot..pivot + 1] {
            assert_eq!(k, val);
        }
        for &j in &data[pivot + 1..] {
            assert!(j >= val);
        }
    }

    #[test]
    fn partition_base_case() {
        let mut data = [1];
        partition(0, &mut data);
        assert_eq!(data, [1]);
    }

    #[test]
    fn partition_all_equal() {
        let mut data = [0, 0, 0, 0, 0];
        partition(1, &mut data);
        assert_eq!(data, [0, 0, 0, 0, 0]);
    }

    #[test]
    fn partition_odd_all_different() {
        let mut data = [1, 3, 5, 2, 4];
        let nth = 1;
        partition(nth, &mut data);
        check_partition(2, nth, &data);
    }

    #[test]
    fn partition_three_groups() {
        let mut data = [3, 2, 3, 2, 1, 2, 1];
        let nth = 3;
        partition(nth, &mut data);
        check_partition(2, nth, &data);
    }

    #[test]
    fn sweep_all_equal() {
        let mut data = [0, 0, 0, 0, 0];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 2);
        assert_eq!(data, [0, 0, 0, 0, 0]);
    }

    #[test]
    fn sweep_sorted() {
        let mut data = [1, 2, 3, 4, 5];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_sorted_even() {
        let mut data = [1, 2, 3, 4, 5, 6];
        let pivot = sweep(3, &mut data);
        assert_eq!(pivot, 3);
        check_partition(4, pivot, &data);
    }

    #[test]
    fn sweep_reverse() {
        let mut data = [5, 4, 3, 2, 1];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_even() {
        let mut data = [6, 5, 4, 3, 2, 1];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 3);
        check_partition(4, pivot, &data);
    }

    #[test]
    fn sweep_sorted_pivot_moved_left() {
        let mut data = [3, 1, 2, 4, 5];
        let pivot = sweep(0, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_sorted_even_pivot_moved_left() {
        let mut data = [3, 1, 2, 4, 5, 6];
        let pivot = sweep(0, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_sorted_pivot_moved_right() {
        let mut data = [1, 2, 4, 5, 3];
        let pivot = sweep(4, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_sorted_even_pivot_moved_right() {
        let mut data = [1, 2, 4, 5, 6, 3];
        let pivot = sweep(5, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_pivot_moved_left() {
        let mut data = [3, 5, 4, 2, 1];
        let pivot = sweep(0, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_even_pivot_moved_left() {
        let mut data = [3, 6, 5, 4, 2, 1];
        let pivot = sweep(0, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_pivot_moved_right() {
        let mut data = [5, 4, 2, 1, 3];
        let pivot = sweep(4, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_even_pivot_moved_right() {
        let mut data = [6, 5, 4, 2, 1, 3];
        let pivot = sweep(5, &mut data);
        assert_eq!(pivot, 2);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_sorted_groups() {
        let mut data = [1, 1, 3, 3, 5, 5];
        let pivot = sweep(2, &mut data);
        assert!(pivot == 2 || pivot == 3);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_reverse_groups() {
        let mut data = [5, 5, 3, 3, 1, 1];
        let pivot = sweep(2, &mut data);
        assert!(pivot == 2 || pivot == 3);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_shuffle_groups() {
        let mut data = [5, 3, 1, 5, 1, 3];
        let pivot = sweep(1, &mut data);
        assert!(pivot == 2 || pivot == 3);
        check_partition(3, pivot, &data);
    }

    #[test]
    fn sweep_pivot_is_largest() {
        let mut data = [2, 4, 5, 1, 3];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 4);
        check_partition(5, pivot, &data);
    }

    #[test]
    fn sweep_pivot_is_smallest() {
        let mut data = [2, 4, 1, 5, 3];
        let pivot = sweep(2, &mut data);
        assert_eq!(pivot, 0);
        check_partition(1, pivot, &data);
    }

    #[test]
    fn sweep_multiple_pivots() {
        let mut data = [2, 1, 2, 3, 2];
        let pivot = sweep(4, &mut data);
        assert_eq!(pivot, 2);
        check_partition(2, pivot, &data);
    }
}

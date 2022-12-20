import "lib/github.com/diku-dk/segmented/segmented"
let newline = u8.i32 '\n'

let ascii_digit_to_i32 (digit: u8): i32 =
	i32.u8 (digit - (u8.i32 '0'))

let split_lines [n] (file: [n]u8): ([]u8, []bool, []i32, []i32) = 
	let file =  (concat_to (n+1) "\n" file) in
	let flags = map (newline ==) file in
	let lengths = segmented_reduce (+) (0) flags (replicate (n+1) 1i32) in
	let offsets = scan (+) (0) lengths in
	(file, flags, lengths, offsets)

let correct_endian_worth_map [n] [m] (flags: [n]bool) (lengths: [m]i32): [n]i32 =
	map (\x -> (lengths[x.1] - (i32.i64 x.0))) (
		let position_worth_wrong_endian: [n]i64 = segmented_iota flags in
		let group_index = replicated_iota (map i32.to_i64 lengths) in
		zip (position_worth_wrong_endian) (group_index :> [n]i64)
		)

let sum_within_groups [n] (values: [n]i32) (lengths: []i32): []i32 =
	let gap_map = map (1 ==) (lengths :> [n]i32) in
	segmented_reduce (+) (0) gap_map values

let  part1__ (file: []u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let endian_worthness_map = correct_endian_worth_map flags lengths in
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x == newline) then 0 else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map in 
	let digit_string_values = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values in
	let group_sums = sum_within_groups digit_string_values lengths in
	let max_sum = i32.maximum group_sums in
		(file, flags, lengths, offsets, endian_worthness_map, worthyness_adjusted_digit_values, digit_string_values, group_sums, max_sum)


let max3 [n] (values: [n]i32): (i32, i32, i32) =
	let neutral = (i32.lowest, i32.lowest, i32.lowest) in
	let merge (a, b, c) (d, e, f) = (
		if a > d then -- a max1; keep
			if b > d then -- b max2; keep
				if c > d then -- c max3; keep
					(a, b, c)
				else -- d max3; keep
					(a, b, d)
			else -- d max2; keep
				if b > e then -- b max3;  keep
					(a, d, b)
				else -- e max3; keep
					(a, d, e)
		else -- d max1; keep
			if e > a then -- e max2; keep
				if f > a then -- f max3; keep
					(d, e, f)
				else -- a max3; keep
					(d, e, a)
			else -- a max2; keep
				if e > b then -- e max3;  keep
					(d, a, e)
				else -- b max3; keep
					(d, a, b)
		) in
	reduce_comm (merge) (neutral) (map (\x -> (x, i32.lowest, i32.lowest)) values)

let part1___ [n] (file: [n]u8) =
	let m = n/4 in
	let unflattened = unflatten m 4 file in
	let (A, B, C, X, Y, Z) = (u8.i32 'A', u8.i32 'B', u8.i32 'C', u8.i32 'X', u8.i32 'Y', u8.i32 'Z') in
	let (R, P, S, Draw, Winn, Loss) = (1, 2, 3, 3, 6, 0) in
	-- X/A : Rock, Y/B : Paper, Z/C: Scissors
	-- Rock > Scissors, Scissors > Paper, Paper > Rock
	let map_mine they me =
		if (me == X && they == A) then (R + Draw) else
		if (me == X && they == B) then (R + Loss) else
		if (me == X && they == C) then (R + Winn) else
		if (me == Y && they == A) then (P + Winn) else
		if (me == Y && they == B) then (P + Draw) else
		if (me == Y && they == C) then (P + Loss) else
		if (me == Z && they == A) then (S + Loss) else
		if (me == Z && they == B) then (S + Winn) else
		if (me == Z && they == C) then (S + Draw) else
		0 in
	let round_scores = map (\(x: [4]u8) -> map_mine x[0] x[2]) unflattened in
	let round_picks = map (\(x: [4]u8) -> ( x[0], x[2])) unflattened in
	(unflattened, round_scores, round_picks)


let part2___ [n] (file: [n]u8) =
	let m = n/4 in
	let unflattened = unflatten m 4 file in
	let (A, B, C, X, Y, Z) = (u8.i32 'A', u8.i32 'B', u8.i32 'C', u8.i32 'X', u8.i32 'Y', u8.i32 'Z') in
	let (R, P, S, Draw, Winn, Loss) = (1, 2, 3, 3, 6, 0) in
	-- A : Rock, B : Paper, C: Scissors
	-- X : Loss, Y : Draw, Z : Win
	-- Rock > Scissors, Scissors > Paper, Paper > Rock
	let map_mine they me =
		if (me == X && they == A) then (S + Loss) else
		if (me == X && they == B) then (R + Loss) else
		if (me == X && they == C) then (P + Loss) else
		if (me == Y && they == A) then (R + Draw) else
		if (me == Y && they == B) then (P + Draw) else
		if (me == Y && they == C) then (S + Draw) else
		if (me == Z && they == A) then (P + Winn) else
		if (me == Z && they == B) then (S + Winn) else
		if (me == Z && they == C) then (R + Winn) else
		0 in
	let round_scores = map (\(x: [4]u8) -> map_mine x[0] x[2]) unflattened in
	let round_picks = map (\(x: [4]u8) -> ( x[0], x[2])) unflattened in
	(unflattened, round_scores, round_picks)



let first_halves_map [n] [m] (flags: [n]bool) (lengths: [m]i32): [n]bool =
	map (\x -> (lengths[x.1] > (2 * ((i32.i64 x.0)-0)))) (
		let position_worth_wrong_endian: [n]i64 = segmented_iota flags in
		let group_index = replicated_iota (map i32.to_i64 lengths) in
		zip (position_worth_wrong_endian) (group_index :> [n]i64)
		)

let part1____ [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let letter_to_bit_masks (letter: u8): (u32, u32) = (
		let case_mask = 32u8 in
		let uppercase_is_zero = case_mask & letter in
		let shift = letter & 31u8 in
		let lower_shift = if uppercase_is_zero == 0 then 0 else shift in
		let upper_shift = if uppercase_is_zero == case_mask then 0 else shift in
		(1u32 << (u32.u8 lower_shift), 1u32 << (u32.u8 upper_shift))
	) in
	let mapped_letters = map (\x -> if x == newline then (0, 0) else letter_to_bit_masks x) file in
	let first_halves = first_halves_map flags lengths in 
	let zipper_letters = zip first_halves mapped_letters in
	let reduced_letters = segmented_reduce (\x y -> ((x.0.0 | y.0.0, x.0.1 | y.0.1), (x.1.0 | y.1.0, x.1.1 | y.1.1)) ) ((1u32, 1u32), (1u32, 1u32)) (flags) (map (\x -> if x.0 then (x.1, (1u32, 1u32)) else ((1u32, 1u32), x.1) ) zipper_letters) -- (thing that splits letter mappings into a first half and a second half branch and keeps them separate until return from this segmented reduce; comparing first with second halves happens AFTER this reduce)
	in let crushed_letters = map (\x -> (x.0.0 & x.1.0, x.0.1 & x.1.1)) reduced_letters in
	let value_rough_letters = map (\x -> ((u32.ctz (x.0 ^ 1u32) |> u32.i32) & !32u32, (u32.ctz (x.1 ^ 1u32) |> u32.i32) & !32u32 )) crushed_letters in
	let priority_values = map (\x -> if x.0 != 0 then x.0 else x.1 + 26) value_rough_letters in
	let priority_sum = u32.sum (init priority_values) in

	(flags, lengths, offsets, first_halves, reduced_letters, crushed_letters, value_rough_letters, priority_values, priority_sum)


let letter_grouper [n] (letters: [n](u32, u32)) =
	let m = n/3 in
	unflatten m 3 letters

let part2____ [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file in
	let letter_to_bit_masks (letter: u8): (u32, u32) = (
		let case_mask = 32u8 in
		let uppercase_is_zero = case_mask & letter in
		let shift = letter & 31u8 in
		let lower_shift = if uppercase_is_zero == 0 then 0 else shift in
		let upper_shift = if uppercase_is_zero == case_mask then 0 else shift in
		(1u32 << (u32.u8 lower_shift), 1u32 << (u32.u8 upper_shift))
	) in
	let mapped_letters = map (\x -> if x == newline then (0, 0) else letter_to_bit_masks x) file in
	--let first_halves = first_halves_map flags lengths in 
	--let zipper_letters = zip first_halves mapped_letters in
	let reduced_letters = segmented_reduce (\x y -> ((x.0 | y.0, x.1 | y.1)) ) (1u32, 1u32) (flags) (mapped_letters) -- (thing that splits letter mappings into a first half and a second half branch and keeps them separate until return from this segmented reduce; comparing first with second halves happens AFTER this reduce)
	in let grouped_letters = letter_grouper (init reduced_letters)
	let crushed_letters = map (\x -> (x[0].0 & x[1].0 & x[2].0, x[0].1 & x[1].1 & x[2].1)) grouped_letters in
	let value_rough_letters = map (\x -> ((u32.ctz (x.0 ^ 1u32) |> u32.i32) & !32u32, (u32.ctz (x.1 ^ 1u32) |> u32.i32) & !32u32 )) crushed_letters in
	let priority_values = map (\x -> if x.0 != 0 then x.0 else x.1 + 26) value_rough_letters in
	let priority_sum = u32.sum (priority_values) in

	(flags, lengths, offsets, reduced_letters, crushed_letters, value_rough_letters, priority_values, priority_sum)


let dash = u8.i32 '-'
let comma = u8.i32 ','

let split_character_day4 (x: u8): bool =
	x == newline || x == dash || x == comma

let split_numbers_for_day4 [n] (file: [n]u8): ([]u8, []bool, []i32, []i32) = 
	let file =  (concat_to (n+1) "\n" file) in
	let flags = map (split_character_day4) file in
	let lengths = segmented_reduce (+) (0) flags (replicate (n+1) 1i32) in
	let offsets = scan (+) (0) lengths in
	(file, flags, lengths, offsets)

let part1_____ [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_numbers_for_day4 file in
	let endian_worthness_map = correct_endian_worth_map flags lengths in
	let worthyness_adjusted_digit_values = map2 (\x y -> if (split_character_day4 x) then 0 else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map in 
	let digit_string_values = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values in
	let useful_values = init digit_string_values in
	let lines = (length useful_values) / 4 in
	let grouped_values = unflatten_3d lines 2 2 useful_values in
	let detect_full_containment (x: [2][2]i32): bool = 
		let x = if x[0,0] < x[1,0] || (x[0,0] == x[1,0] && x[0,1] > x[1,1]) then x[:] else x[::-1] in
		x[0,1] >= x[1,1]
	in
	let detect_overlap (x: [2][2]i32): bool =
		(x[1,0] <= x[0,0] && x[0,0] <= x[1,1]) || -- start of x[0] overlaps with x[1]
		(x[0,0] <= x[1,0] && x[1,0] <= x[0,1])    -- start of x[1] overlaps with x[0]
	in
	let detected_containment = map detect_full_containment grouped_values in
	let detected_overlap = map detect_overlap grouped_values in
	let count_fully_contained = length (filter (id) detected_containment) in
	let count_overlapping = length (filter (id) detected_overlap) in
		(file, flags, lengths, offsets, endian_worthness_map, worthyness_adjusted_digit_values, digit_string_values, lines, grouped_values, detected_containment, count_fully_contained, detected_overlap, count_overlapping)

let part1_6 [n] (file: [n]u8) =
	let mapped = map5 (\a b c d e -> if (a != b && a != c && a != d && b != c && b != d && c != d) then e else i64.highest) file (rotate (-1) file) (rotate (-2) file) (rotate (-3) file) (indices file)
	let sliced = (.[3:]) mapped
	let reduced = reduce (i64.min) (i64.highest) sliced
	let added = (+ 1) reduced
	let test_mapped = [file, (rotate (-1) file), (rotate (-2) file), (rotate (-3) file)]
	in (mapped, sliced, reduced, added, test_mapped)

let part2_6 [n] (file: [n]u8) =
	let f0 = (rotate (-1) file)
	let f1 = (rotate (-1) f0)
	let f2 = (rotate (-1) f1)
	let f3 = (rotate (-1) f2)
	let f4 = (rotate (-1) f3)
	let f5 = (rotate (-1) f4)
	let f6 = (rotate (-1) f5)
	let f7 = (rotate (-1) f6)
	let f8 = (rotate (-1) f7)
	let f9 = (rotate (-1) f8)
	let f10 = (rotate (-1) f9)
	let f11 = (rotate (-1) f10)
	let f12 = (rotate (-1) f11)
	let map_fn a b c d e f g h i j k l m n: bool =
		a != b && a != c && a != d && a != e && a != f && a != g && a != h && a != i && a != j && a != k && a != l && a != m && a != n &&
		b != c && b != d && b != e && b != f && b != g && b != h && b != i && b != j && b != k && b != l && b != m && b != n &&
		c != d && c != e && c != f && c != g && c != h && c != i && c != j && c != k && c != l && c != m && c != n &&
		d != e && d != f && d != g && d != h && d != i && d != j && d != k && d != l && d != m && d != n &&
		e != f && e != g && e != h && e != i && e != j && e != k && e != l && e != m && e != n &&
		f != g && f != h && f != i && f != j && f != k && f != l && f != m && f != n &&
		g != h && g != i && g != j && g != k && g != l && g != m && g != n &&
		h != i && h != j && h != k && h != l && h != m && h != n &&
		i != j && i != k && i != l && i != m && i != n &&
		j != k && j != l && j != m && j != n &&
		k != l && k != m && k != n &&
		l != m && l != n &&
		m != n
	let mapped = map4 (\x y z u -> if (map_fn x.0 x.1 x.2 x.3 x.4 y.0 y.1 y.2 y.3 y.4 z.0 z.1 z.2 z.3) then u else i64.highest) (zip5 file f0 f1 f2 f3) (zip5 f4 f5 f6 f7 f8) (zip4 f9 f10 f11 f12) (indices file)
	let sliced = (.[13:]) mapped
	let reduced = reduce (i64.min) (i64.highest) sliced
	let added = (+ 1) reduced
	in (mapped, sliced, reduced, added)

let part1_8 [n] (file: [n]u8) =
	let line_count = map ((== newline) >-> i32.bool) file |> reduce (+) 0 |> i64.i32
	let line_length = assert (n % line_count == 0) (n / line_count)
	let raw_grid = unflatten line_count line_length file
	let mapped_raw_grid = map (map (ascii_digit_to_i32 >-> u8.i32)) raw_grid
	let valid_grid = raw_grid[:,:(line_length - 1)]
	let valid_mapped_grid = mapped_raw_grid[:,:(line_length - 1)]
	let line_to_flags (line) =
		let max_scanned = scan (u8.max) u8.lowest line
		let compared =  map2 (>) (rotate 1 line) max_scanned
		in (rotate (-1) compared) with [0] = true
	let left_to_right_flags = map line_to_flags valid_grid
	let generate_flags [a] [b] (grid: [a][b]u8): [a][b]bool =
		let flag_generator [c] [d] 't 'u
		(forwards: ([a][b]u8 -> [c][d]u8)) (backwards: ([c][d]bool -> [a][b]bool)) =
			(forwards >-> map line_to_flags >-> backwards) grid
		let flag_grid = map4 (\e f g h -> map4 (\a b c d -> a || b || c || d) e f g h)
			(flag_generator id id)
			(flag_generator (map (reverse)) (map (reverse)))
			(flag_generator (transpose) (transpose))
			(flag_generator (transpose >-> (map (reverse))) (transpose <-< (map (reverse))))
		in flag_grid
	let flag_grid = generate_flags valid_grid
	let count = flatten flag_grid |> map (i64.bool) |> i64.sum
	in (line_count, line_length, raw_grid, mapped_raw_grid, valid_mapped_grid, left_to_right_flags, flag_grid, count)

let part2_8 [n] (file: [n]u8) =
	let line_count = map ((== newline) >-> i32.bool) file |> reduce (+) 0 |> i64.i32
	let line_length = assert (n % line_count == 0) (n / line_count)
	let raw_grid = unflatten line_count line_length file
	let mapped_raw_grid = map (map (ascii_digit_to_i32 >-> u8.i32)) raw_grid
	let valid_mapped_grid = mapped_raw_grid[:,:(line_length - 1)]
	let line_beam (height) (line) =
		map (>= height) line |>
		segmented_iota |>
		map ((+ 1) >-> u16.i64) |>
		rotate (-1)
	let generate_beams [a] [b] (height) (grid: [a][b]u8): [a][b]u64 =
		let beam_generator [c] [d] 't 'u
		(forwards: ([a][b]u8 -> [c][d]u8)) (backwards: ([c][d]u16 -> [a][b]u16)) =
			(forwards >-> map (line_beam height) >-> backwards) grid
		let beam_grid = map4 (\e f g h -> map4 (\a b c d -> (u64.u16 a) * (u64.u16 b) * (u64.u16 c) * (u64.u16 d)) e f g h)
			(beam_generator id id)
			(beam_generator (map (reverse)) (map (reverse)))
			(beam_generator (transpose) (transpose))
			(beam_generator (transpose >-> (map (reverse))) (transpose <-< (map (reverse))))
		in beam_grid
	let beam_grid = map (\x -> generate_beams (u8.i64 x) valid_mapped_grid) (iota 10)
	let scores_pre_map = (map (transpose) (transpose beam_grid))
	let scores = map2 (\beams_line tree_line -> map2 (\(beams : [10]u64) (tree : u8) -> beams[i64.u8 tree]) beams_line tree_line) (scores_pre_map) valid_mapped_grid
	let max_score_fn [a] [b] (scores: [a][b]u64) : u64 =
		let am = a - 1
		let bm = b - 1
		let cropped_scores = scores[1:am,1:bm]
		in flatten cropped_scores |> u64.maximum
	let max_score = max_score_fn scores
	in (line_count, line_length, raw_grid, mapped_raw_grid, valid_mapped_grid, beam_grid, scores, max_score)

let part1_10 [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file
	let endian_worthness_map = correct_endian_worth_map flags lengths
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x <= u8.i32 '0' || x > u8.i32 '9') then (if (x == u8.i32 '-') then i32.lowest else 0) else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map
	let [lines] digit_string_values: [lines]i32 = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values
	let [m] instructions: [m](#addx i32 | #noop) = init (map2 (\x y -> if x < 0 then #addx (-(x + i32.lowest)) else if y == 5 then #noop else #addx x) digit_string_values (lengths :> [lines]i32))
	let diffs = expand (\x -> match x case #addx _ -> 2 case #noop -> 1) (\x i -> if i == 0 then 0 else match x case #addx diff -> diff case #noop -> 0) instructions
	let diffs_with_initial = map2 (\x i -> if i == 0 then 1 else x) diffs (indices diffs)
	let x_vals = scan (+) 0 diffs_with_initial
	let signal_strength = map2 (\x i -> x * ((i32.i64 i)+2)) x_vals (indices diffs)
	let probe i = signal_strength[i-2]
	let signal_strength_sum_over_targets = probe 20 + probe 60 + probe 100 + probe 140 + probe 180 + probe 220
	in (instructions, transpose [diffs_with_initial, x_vals, signal_strength, map ((+ 2)>->i32.i64) (indices diffs)], probe 20 , probe 60 , probe 100 , probe 140 , probe 180 , probe 220, signal_strength_sum_over_targets, x_vals, diffs_with_initial)

let part2_10 [n] (file: [n]u8) =
	let (_, _, _, _, _, _, _, _, _, x_vals, x_vals_offsets) = part1_10 file
	let len_xvals = length x_vals
	let x_grid_raw = unflatten 6 40 x_vals
	let x_offsets_grid = unflatten 6 40 x_vals_offsets
	let x_grid = map2 (map2 (-)) x_grid_raw x_offsets_grid
	let pixels = map2 (\row y -> map2 (\v x -> ((i32.i64 x) >= (v-1) && (i32.i64 x) <= (v+1))) row (indices row)) x_grid (indices x_grid)
	let line_strings = map (map (\x -> u8.i32 (if x then '#' else '.'))) pixels
	let formatted_string = transpose line_strings |> (++ [(replicate 6 newline)]) |> transpose |> flatten
	in (x_vals, len_xvals, x_grid, pixels, formatted_string)

let part1_20 [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file
	let endian_worthness_map = correct_endian_worth_map flags lengths
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x <= u8.i32 '0' || x > u8.i32 '9') then (if (x == u8.i32 '-') then i32.lowest else 0) else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map
	let [lines] digit_string_values: [lines]i32 = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values
	let [m] instructions: [m](i64) = init (map (\x-> if x < 0 then (-(x + i32.lowest)) else x) digit_string_values) |> map i64.i32
	let search_i16 (needle: i16) (haystack: []i16): i16 =
		map2 (\x i -> if x == needle then (i16.i64 i) else i16.highest) haystack (indices haystack) |>
		i16.minimum
	let search_i64 (needle: i64) (haystack: []i64): i64 =
		map2 (\x i -> if x == needle then i else i64.highest) haystack (indices haystack) |>
		i64.minimum
	let permutation = loop permutation = (iota m |> map i16.i64) for i < m do
		let small_i = i16.i64 i
		let i_pos = search_i16 small_i permutation |> i64.i16
		let permutation = (rotate (1 + i_pos)) permutation
		let mod_shift = instructions[i] % (m -1)
		in (permutation[:mod_shift] ++ [small_i] ++ (init permutation[mod_shift:])) :> [m]i16
	let mix_result = map (\i -> instructions[i]) permutation
	let pos_0 = search_i64 0 mix_result
	let mix_result = rotate pos_0 mix_result
	let coord_1 = mix_result[1000 % m]
	let coord_2 = mix_result[2000 % m]
	let coord_3 = mix_result[3000 % m]
	let coord_sum = coord_1 + coord_2 + coord_3
	in (lengths, digit_string_values, instructions, permutation, mix_result, pos_0, coord_1, coord_2, coord_3, coord_sum)

let part2_20 [n] (file: [n]u8) =
	let (file, flags, lengths, offsets) = split_lines file
	let endian_worthness_map = correct_endian_worth_map flags lengths
	let worthyness_adjusted_digit_values = map2 (\x y -> if (x <= u8.i32 '0' || x > u8.i32 '9') then (if (x == u8.i32 '-') then i32.lowest else 0) else ((ascii_digit_to_i32 x) * (10 ** (y - 1)))) file endian_worthness_map
	let [lines] digit_string_values: [lines]i32 = segmented_reduce (+) (0) flags worthyness_adjusted_digit_values
	let [m] instructions_raw: [m](i64) = init (map (\x-> if x < 0 then (-(x + i32.lowest)) else x) digit_string_values) |> map (i64.i32)
	let instructions = map (* 811589153) instructions_raw
	let search_i16 (needle: i16) (haystack: []i16): i16 =
		map2 (\x i -> if x == needle then (i16.i64 i) else i16.highest) haystack (indices haystack) |>
		i16.minimum
	let search_i64 (needle: i64) (haystack: []i64): i64 =
		map2 (\x i -> if x == needle then i else i64.highest) haystack (indices haystack) |>
		i64.minimum
	let permutation = loop permutation = (iota m |> map i16.i64) for j < 10 do (
		loop permutation for i < m do
			let small_i = i16.i64 i
			let i_pos = search_i16 small_i permutation |> i64.i16
			let permutation = (rotate (1 + i_pos)) permutation
			let mod_shift = instructions[i] % (m -1)
			in (permutation[:mod_shift] ++ [small_i] ++ (init permutation[mod_shift:])) :> [m]i16
		)
	let mix_result = map (\i -> instructions[i]) permutation
	let pos_0 = search_i64 0 mix_result
	let mix_result = rotate pos_0 mix_result
	let coord_1 = mix_result[1000 % m]
	let coord_2 = mix_result[2000 % m]
	let coord_3 = mix_result[3000 % m]
	let coord_sum = coord_1 + coord_2 + coord_3
	in (lengths, digit_string_values, instructions, permutation, mix_result, pos_0, coord_1, coord_2, coord_3, coord_sum)

entry part1 (file: []u8): u32 =
	u32.i64 (part1_20 file).9

entry part2 (file: []u8): u64 =
	u64.i64 (part2_20 file).9

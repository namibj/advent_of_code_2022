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


module day5 = {
	module type move_compounding = {
		type crate_idx

--		val compound_move [crates] [stacks]
		type compound_move [crates] [stacks] = {
			crate_map: [crates]crate_idx,
			start_stack_heights: [stacks]crate_idx,
			end_stack_heights: [stacks]crate_idx -- ,
			--valid_crates: crate_idx -- implicitly (sum *_stack_heights)
		}

		val expand_primitive_move [crates] [stacks]: (count: crate_idx) -> (from: crate_idx) -> (to: crate_idx) -> (compound_move [crates] [stacks])
		val chain_compound_moves [crates] [stacks]: (first: compound_move [crates] [stacks]) -> (second: compound_move [crates] [stacks]) -> (compound_move [crates] [stacks])
	}
	module mk_move_compounding (crate_idx_m: integral) : move_compounding with crate_idx = crate_idx_m.t = {
		type crate_idx = crate_idx_m.t
		type compound_move [crates] [stacks] = {
			crate_map: [crates]crate_idx,
			start_stack_heights: [stacks]crate_idx,
			end_stack_heights: [stacks]crate_idx -- ,
			--valid_crates: crate_idx -- implicitly (sum *_stack_heights)
		}

		local let to_i64 = crate_idx_m.to_i64
		local let from_i64 = crate_idx_m.i64
		local let idx_0 = crate_idx_m.u8 0
		local let cim_p a b = crate_idx_m.(a + b)

		let expand_primitive_move [crates] [stacks] (count: crate_idx) (from: crate_idx) (to: crate_idx) : compound_move [crates] [stacks] = 
			let count_ = to_i64 count
			let from_ = to_i64 from - 1
			let crate_map = replicate crates idx_0
			let to_ = to_i64 to - 1
			in {
				crate_map = crate_map with [:count_] = map crate_idx_m.i64 (reverse (iota count_)),
				start_stack_heights = (replicate stacks idx_0) with [from_] = count,--concat_to stacks ((replicate from_ idx_0) ++ [count]) (replicate (stacks - from_ - 1) idx_0),
				end_stack_heights = (replicate stacks idx_0) with [to_] = count--concat_to stacks ((replicate to_ idx_0) ++ [count]) (replicate (stacks - to_ - 1) idx_0)
			}

		local type input_slot_disposition = #empty | #first_i {stack: crate_idx, depth: crate_idx} | #second_i {stack: crate_idx, depth: crate_idx}
		local type output_slot_origin = #empty | #first_o {stack: crate_idx, depth: crate_idx} | #second_o {stack: crate_idx, depth: crate_idx}

		let chain_compound_moves [crates] [stacks] (first: compound_move [crates] [stacks]) (second: compound_move [crates] [stacks]): (compound_move [crates] [stacks]) =
			let offsets_1 = scan (cim_p) idx_0 first.start_stack_heights
			let offsets_2 = scan (cim_p) idx_0 first.end_stack_heights
			let offsets_3 = scan (cim_p) idx_0 second.start_stack_heights
			let offsets_4 = scan (cim_p) idx_0 second.end_stack_heights
			let stack_map_create (heights: [stacks]crate_idx): [crates]i64 =
				let mapped_heights = map (to_i64) heights
				let sum_heights = (reduce (+) 0 mapped_heights)
				in (replicate crates 0) with [0:sum_heights] = replicated_iota mapped_heights
			let stack_map_1 = -- replicated_iota (map (to_i64)
			stack_map_create first.start_stack_heights-- ) :> [crates]i64
			let stack_map_2 = -- replicated_iota (map (to_i64)
			stack_map_create first.end_stack_heights-- ) :> [crates]i64
			let stack_map_3 = -- replicated_iota (map (to_i64)
			stack_map_create second.start_stack_heights-- ) :> [crates]i64
			let stack_map_4 = -- replicated_iota (map (to_i64)
			stack_map_create second.end_stack_heights-- ) :> [crates]i64
			let input_thickness_usage = map3 (\x y z -> crate_idx_m.(if z > y then (x, z - y, x + z - y) else (x, idx_0, x))) first.start_stack_heights first.end_stack_heights second.start_stack_heights
			-- (thickness_taken_by_first, thickness_taken_by_second_but_left_untouched_by_first, total_input_thickness_taken)
			let output_thickness_usage = map3 (\x y z -> crate_idx_m.(if x > y then (x - y, z, x - y + z) else (idx_0, z, z))) first.end_stack_heights second.start_stack_heights second.end_stack_heights
			-- (thickness_produced_by_first_but_left_untouched_by_second, thickness_produced_by_second, total_output_thickness_produced)
			let input_heights = (unzip3 input_thickness_usage).2
			let output_heights = (unzip3 output_thickness_usage).2
			let offsets_5 = scan (cim_p) idx_0 input_heights
			let offsets_6 = scan (cim_p) idx_0 output_heights
			let stack_map_5 = -- replicated_iota (map (to_i64)
			stack_map_create input_heights-- ) :> [crates]i64
			let stack_map_6 = -- replicated_iota (map (to_i64)
			stack_map_create output_heights-- ) :> [crates]i64
			let input_disposition_map =
				let map_fn (x) (i: i64): input_slot_disposition =
					let i_c = from_i64 i in
					if (i < (to_i64 x.0.0))
						then #first_i {stack = from_i64 x.3, depth = i_c}
						else #second_i {stack = from_i64 x.3, depth = crate_idx_m.(i_c - x.0.0 + x.2)}
				let tmp = expand ((.0.2) >-> to_i64) (map_fn) (zip4 input_thickness_usage output_thickness_usage first.end_stack_heights (iota stacks))
				in concat_to crates tmp (replicate (crates - (length tmp)) #empty)
			let output_origin_map =
				let map_fn (x) (i: i64): output_slot_origin =
					let i_c = from_i64 i in
					if (i < (to_i64 x.1.1))
						then #second_o {stack = from_i64 x.3, depth = i_c}
						else #first_o {stack = from_i64 x.3, depth = crate_idx_m.(i_c - x.1.1 + x.2)}
				let tmp = expand ((.1.2) >-> to_i64) (map_fn) (zip4 input_thickness_usage output_thickness_usage second.start_stack_heights (iota stacks))
				in concat_to crates tmp (replicate (crates - (length tmp)) #empty)
			let output_map_fn (x: input_slot_disposition): crate_idx =
				match x
				case #empty -> idx_0
				case #first_i outer_input_disposition ->
					let oid_stack = outer_input_disposition.stack
					let oid_depth = outer_input_disposition.depth
					let first_mapped_pos = first.crate_map[to_i64 crate_idx_m.(offsets_1[to_i64 oid_stack] - first.start_stack_heights[to_i64 oid_stack] + oid_depth)]
					let fo_stack = stack_map_2[to_i64 first_mapped_pos]
					let fo_depth = crate_idx_m.(first_mapped_pos - (offsets_2[fo_stack] - first.end_stack_heights[fo_stack]))
					in if crate_idx_m.(fo_depth < second.start_stack_heights[fo_stack])
						then -- apply_second_map
							let si_stack = fo_stack
							let si_depth = fo_depth
							let second_mapped_pos = second.crate_map[to_i64 crate_idx_m.(offsets_3[si_stack] - second.start_stack_heights[si_stack] + si_depth)]
							let so_stack = stack_map_4[to_i64 second_mapped_pos]
							let so_depth = crate_idx_m.(second_mapped_pos - (offsets_4[so_stack] - second.end_stack_heights[so_stack]))
							let oo_stack = so_stack
							let oo_depth = so_depth
							in crate_idx_m.(offsets_6[oo_stack] - output_heights[oo_stack] + oo_depth)
						else -- passes_through_to_output_without_getting_touched_by_second
							let oo_stack = fo_stack
							let oo_depth = crate_idx_m.(fo_depth - second.start_stack_heights[fo_stack] + output_thickness_usage[oo_stack].1)
							in crate_idx_m.(offsets_6[oo_stack] - output_heights[oo_stack] + oo_depth)
				case #second_i outer_input_disposition ->
					let oid_stack = outer_input_disposition.stack
					let oid_depth = outer_input_disposition.depth
					let si_stack = oid_stack
					let si_depth = oid_depth
					let second_mapped_pos = second.crate_map[to_i64 crate_idx_m.(offsets_3[to_i64 si_stack] - second.start_stack_heights[to_i64 si_stack] + si_depth)]
					let so_stack = stack_map_4[to_i64 second_mapped_pos]
					let so_depth = crate_idx_m.(second_mapped_pos - (offsets_4[so_stack] - second.end_stack_heights[so_stack]))
					let oo_stack = so_stack
					let oo_depth = so_depth
					in crate_idx_m.(offsets_6[oo_stack] - output_heights[oo_stack] + oo_depth)
			let crate_map = map output_map_fn input_disposition_map
			let start_stack_heights = input_heights
			let end_stack_heights = output_heights
			in {crate_map, start_stack_heights, end_stack_heights}
			--in expand_primitive_move idx_0 idx_0 idx_0
	}

	module move_u8 = mk_move_compounding u8
	open move_u8
	type specific_compound_move = {
		crate_map: [56]u8,
		start_stack_heights: [9]u8,
		end_stack_heights: [9]u8 -- ,
		--valid_crates: crate_idx -- implicitly (sum *_stack_heights)
	}

	let part1_ [n] (file: [n]u8) =
		let (file, flags, lengths, offsets) = split_lines file
		let length_of_header = map2 (\x y -> if x == 1 then y else i32.highest) lengths offsets |> i32.minimum |> (+ -1) |> i32.to_i64
		--let lines_of_header = map2 (\x y -> if x == 1 then y else i64.highest) lengths (indices lengths) |> i64.minimum
		let lines_of_header = (length_of_header / (i32.to_i64 lengths[0]))
		let stacks = ((i32.to_i64 lengths[0])/4)
		let header = unflatten_3d lines_of_header stacks 4 file[:length_of_header] |> transpose
		let initial_max_stack_height = lines_of_header - 1
		let stacks_start_raw = header[:,:initial_max_stack_height,2]
		let initial_stacks_emptynesses = map (map ((== (u8.i32 ' ')) >-> i64.bool) >-> i64.sum) stacks_start_raw
		let initial_stacks_contents = map2 (rotate) initial_stacks_emptynesses stacks_start_raw
		let parse_move (offset) (length) =
			let offset = i32.to_i64 offset
			let length = i32.to_i64 length
			let tail = offset + 20
			let data = (file[offset:tail] :> [20]u8) |> (map (ascii_digit_to_i32 >-> u8.i32))
			in if length == 20
				then (data[6] * 10 + data[7], data[14], data[19])
				else (data[6], data[13], data[18])
		let start_of_moves: i64 = lines_of_header + 1
		let moves = map (uncurry parse_move) ((zip (rotate (-1) offsets)[start_of_moves:] lengths[start_of_moves:]) |> init)
		let expand_uncurry_move (count: u8) (from: u8) (to: u8): specific_compound_move =
			expand_primitive_move count from to
		let expanded_moves: []specific_compound_move = map (\x -> expand_uncurry_move x.0 x.1 x.2) moves
		let zero_move = {crate_map = replicate 56 0, start_stack_heights = replicate 9 0, end_stack_heights = replicate 9 0}
		let combined_move = reduce (chain_compound_moves) (zero_move) expanded_moves
		let exclusive_prefix_sum (input) = (scan (+) (0) input) |> map2 (flip (-)) input
		let offsets_touched_starts = exclusive_prefix_sum combined_move.start_stack_heights
		let offsets_touched_ends = exclusive_prefix_sum combined_move.end_stack_heights
		let want_slots_post_mapping = offsets_touched_ends
		let valid_map_length = offsets_touched_starts[stacks - 1] + combined_move.start_stack_heights[stacks - 1] |> i64.u8
		let crate_map = combined_move.crate_map
		let crates = 56
		let inverse_crate_map = scatter (replicate crates 0) (map (i64.u8) crate_map[:valid_map_length]) (iota valid_map_length)
		let want_slots_pre_mapping = map (\x -> inverse_crate_map[i64.u8 x]) want_slots_post_mapping
		let stacks_ = 9
		let stack_map_create (heights: [stacks_]u8): [crates]i64 =
			let mapped_heights = map (i64.u8) heights
			let sum_heights = (reduce (+) 0 mapped_heights)
			in (replicate crates 0) with [0:sum_heights] = replicated_iota mapped_heights
		let stack_map_touched_starts = stack_map_create (combined_move.start_stack_heights :> [stacks_]u8)
		let want_stacks_post_mapping = iota stacks
		let want_stacks_pre_mapping = map (\x -> stack_map_touched_starts[want_slots_pre_mapping[x]]) want_stacks_post_mapping
		let crates_on_tops_of_stacks = map2 (\x y -> initial_stacks_contents[x, (y - i64.u8 offsets_touched_starts[x]) ] ) want_stacks_pre_mapping want_slots_pre_mapping[:stacks]
		in	(--file, flags, lengths, offsets,
		length_of_header, --header,
		lines_of_header, --moves, expanded_moves,
		stacks,
		stacks_start_raw,
		initial_stacks_emptynesses,
		initial_stacks_contents,
		offsets_touched_starts,
		offsets_touched_ends,
		valid_map_length,
		want_slots_pre_mapping,
		inverse_crate_map,
		stack_map_touched_starts,
		want_stacks_post_mapping,
		want_stacks_pre_mapping,
		crates_on_tops_of_stacks,
		combined_move)

	--, endian_worthness_map, worthyness_adjusted_digit_values, digit_string_values, group_sums, max_sum)
}

let part1_ = day5.part1_

entry part1 (file: []u8): []u8 =
	(part1_ file).14

entry part2 (file: []u8): u32 =
	u32.i64 (part1_____ file).12

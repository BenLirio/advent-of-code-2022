; Input file path
@.input_file_name = private constant [10 x i8] c"input.txt\00"
@.input_small_file_name = private constant [16 x i8] c"input_small.txt\00"
@.print_pair_format = private constant [17 x i8] c"[%d %d] [%d %d]\0A\00"
@.print_output_format = private constant [4 x i8] c"%d\0A\00"

define i1 @pair_overlapped(i32 %fs, i32 %fe, i32 %ss, i32 %se) {
  %fs_lte_ss = icmp sle i32 %fs, %ss
  br i1 %fs_lte_ss, label %assume_fs_lte_ss, label %flip_pairs
assume_fs_lte_ss:
  %fe_gte_ss = icmp sge i32 %fe, %ss
  ret i1 %fe_gte_ss
flip_pairs:
  %flipped_output = call i1 @pair_overlapped(i32 %ss, i32 %se, i32 %fs, i32 %fe)
  ret i1 %flipped_output
}

define i32 @main() {
  %ok_ptr = alloca i1
  %acc_ptr = alloca i32
  %pair = alloca [4 x i32]
  %pair_idx_ptr = alloca i32
  %num_overlapped_ptr = alloca i32

  store i1 1, i1* %ok_ptr
  store i32 0, i32* %acc_ptr
  store i32 0, i32* %pair_idx_ptr
  store i32 0, i32* %num_overlapped_ptr

  %fd = call i32 (i8*, i32, ...) @open(i8* getelementptr ([10 x i8], [10 x i8]* @.input_file_name, i64 0, i64 0), i32 0)
  ; %fd = call i32 (i8*, i32, ...) @open(i8* getelementptr ([16 x i8], [16 x i8]* @.input_small_file_name, i64 0, i64 0), i32 0)
  %open_failed = icmp eq i32 %fd, -1
  br i1 %open_failed, label %failed, label %open_succeeded
open_succeeded:
  %buf = alloca [10 x i8]
  %buf_ptr = getelementptr [10 x i8], [10 x i8]* %buf, i64 0, i64 0
  br label %read_loop
read_loop:
  %num_read = call i32 (i32, i8*, i32) @read(i32 %fd, i8* %buf_ptr, i32 10)
  %read_failed = icmp eq i32 %num_read, -1
  br i1 %read_failed, label %cleanup, label %read_succeeded
read_succeeded:
  %read_complete = icmp eq i32 %num_read, 0
  br i1 %read_complete, label %print_output, label %use_buffer
use_buffer:
  br label %main_loop_setup

main_loop_setup:
  %i_ptr = alloca i32
  store i32 0, i32* %i_ptr
  br label %main_loop_body
main_loop_body:
  %i = load i32, i32* %i_ptr
  %i_as_i64 = sext i32 %i to i64
  %buf_cur_ptr = getelementptr [10 x i8], [10 x i8]* %buf, i64 0, i64 %i_as_i64
  %cur_char = load i8, i8* %buf_cur_ptr
  br label %next_char
next_char_done:
  %i_next = add i32 %i, 1
  store i32 %i_next, i32* %i_ptr
  %buf_used = icmp eq i32 %i_next, %num_read
  br i1 %buf_used, label %read_loop, label %main_loop_body

next_char:
  ; call i32 @write(i32 1, i8* %buf_cur_ptr, i32 1)
  %is_newline = icmp eq i8 %cur_char, 10
  %is_comma = icmp eq i8 %cur_char, 44
  %is_dash = icmp eq i8 %cur_char, 45
  %is_separator0 = or i1 %is_newline, %is_comma
  %is_separator = or i1 %is_separator0, %is_dash
  br i1 %is_separator, label %output_number, label %inc_acc
inc_acc:
  %acc = load i32, i32* %acc_ptr
  %acc0 = mul i32 %acc, 10
  %digit_val = sub i8 %cur_char, 48
  %digit_val_as_i32 = sext i8 %digit_val to i32
  %acc1 = add i32 %acc0, %digit_val_as_i32
  store i32 %acc1, i32* %acc_ptr
  br label %next_char_done

output_number:
  %num = load i32, i32* %acc_ptr
  %pair_idx = load i32, i32* %pair_idx_ptr
  %cur_pair_elem_ptr = getelementptr [4 x i32], [4 x i32]* %pair, i64 0, i32 %pair_idx
  store i32 %num, i32* %cur_pair_elem_ptr
  store i32 0, i32* %acc_ptr
  %pair_idx_next = add i32 %pair_idx, 1
  %pair_idx_next2 = srem i32 %pair_idx_next, 4
  store i32 %pair_idx_next2, i32* %pair_idx_ptr
  br i1 %is_newline, label %output_pair, label %next_char_done
output_pair:
  %first_start_ptr = getelementptr [4 x i32], [4 x i32]* %pair, i64 0, i32 0
  %first_start = load i32, i32* %first_start_ptr
  %first_end_ptr = getelementptr [4 x i32], [4 x i32]* %pair, i64 0, i32 1
  %first_end = load i32, i32* %first_end_ptr
  %second_start_ptr = getelementptr [4 x i32], [4 x i32]* %pair, i64 0, i32 2
  %second_start = load i32, i32* %second_start_ptr
  %second_end_ptr = getelementptr [4 x i32], [4 x i32]* %pair, i64 0, i32 3
  %second_end = load i32, i32* %second_end_ptr
  %pair_is_overlapped = call i1 @pair_overlapped(i32 %first_start, i32 %first_end, i32 %second_start, i32 %second_end)
  br i1 %pair_is_overlapped, label %print_pair, label %done_print_pair
done_print_pair:
  %num_overlapped = load i32, i32* %num_overlapped_ptr
  %pair_is_overlapped_as_i32 = zext i1 %pair_is_overlapped to i32
  %num_overlapped_next = add i32 %num_overlapped, %pair_is_overlapped_as_i32
  store i32 %num_overlapped_next, i32* %num_overlapped_ptr
  br label %next_char_done

print_pair:
  call i32 (i8*, ...) @printf(i8* getelementptr ([17 x i8], [17 x i8]* @.print_pair_format, i64 0, i64 0), i32 %first_start, i32 %first_end, i32 %second_start, i32 %second_end)
  br label %done_print_pair

print_output:
  %final_num_overlapped = load i32, i32* %num_overlapped_ptr
  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.print_output_format, i64 0, i64 0), i32 %final_num_overlapped)
  br label %cleanup
cleanup:
  call i32 @close(i32 %fd)
  %ok = load i1, i1* %ok_ptr
  br i1 %ok, label %done, label %failed
done:
  call void @exit(i32 0)
  unreachable
failed:
  call void @exit(i32 1)
  unreachable
}

declare i32 @open(i8*, i32, ...)
declare void @exit(i32)
declare i32 @read(i32, i8*, i32)
declare i32 @close(i32)
declare i32 @write(i32, i8*, i32)
declare i32 @printf(i8*, ...)
echo $1
# TODO input multi file number and concat them all.
# read -p "Please the number of videos that you want to concat: " n

# https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
# The meaning of %P in find of -printf option https://unix.stackexchange.com/questions/215234/explanation-of-directives-in-find-printf
# TODO learn about `find` command.
ffmpeg_file_list=$(find . -type f -name 'G*'${n}'.MP4' -printf $'file %P\n' | sort) 

echo "$ffmpeg_file_list" > temp.tmp

echo ffmpeg_file_list: $(cat temp.tmp)


echo begin run ffmpeg!

# ffmpeg -f concat -i <<<"$ffmpeg_file_list" -c copy $n.mkv
# TODO: using string replace the temp.tmp file.
# TODO: add date information in output file name.
# TODO mv chaptered files after concat.
# ffmpeg -f concat -i temp.tmp -c copy $n.mkv

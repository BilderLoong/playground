#!/bin/bash
# TODO input multi file number and concat them all.
file_list_file=.${1}.tmp

ffmpeg_file_list=$get_file_list $1

echo "$ffmpeg_file_list" > $file_list_file

echo ffmpeg_file_list: $(cat $file_list_file)


echo begin run ffmpeg!

# ffmpeg -f concat -i <<<"$ffmpeg_file_list" -c copy $n.mkv
# TODO: using string replace the temp.tmp file.
# TODO: add date information in output file name.
# TODO mv chaptered files after concat.
# ffmpeg -f concat -i $file_list_file -c copy $n.mkv


get_file_list() {
    # https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
    # The meaning of %P in find of -printf option https://unix.stackexchange.com/questions/215234/explanation-of-directives-in-find-printf
    # TODO learn about `find` command.
    ffmpeg_file_list=$(find . -type f -name 'G*'${1}'.MP4' -printf 'file %P\n' | sort)
    return ffmpeg_file_list
}

tes=$is_charptered_video
echo $tes

is_charptered_video(){
    count=find G*${1}.MP4 | wc -l
    return cout == 1
}

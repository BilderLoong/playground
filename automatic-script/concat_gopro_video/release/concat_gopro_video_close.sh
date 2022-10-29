#!/bin/bash
# Generate the file list to be concat in the format used by ffmpeg.
# ARGUEMNTS:
#   Accept one parameter which should be the video indentifier.
# OUTPUT:
#   A string which is a file list of video files to be concated in the format used by ffmpeg.
get_file_list() {
    # https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
    # The meaning of %P in find of -printf option https://unix.stackexchange.com/questions/215234/explanation-of-directives-in-find-printf
    # TODO learn about `find` command.
    local ffmpeg_file_list=$(find . -type f -name "G*${1}.MP4" -printf "file %P\n" | sort)
    echo "$ffmpeg_file_list"
}

# Check whether a video is charpterd.
# ARGUEMNTS:
#   Accept one parameter which should be the video identifier.
# OUTPUT:
#   1: there are multiple charpters of the given video.
#   0: there is only one charpter of the given video.
is_charptered_video() {
    count=$(find . -type f -name "G*${1}.MP4" -printf "." | wc -m)
    echo $(( count > 1 ))
}

# Get the video identifier from full file name.
# ARGUEMNTS:
#   Accept one parameter contains a video's full name.
# OUTPUT:
#   The identifier of the given video.
# Example:
#   get_video_identifier GX010002.MP4 should output 0002.
# TODO: findish this function: https://stackoverflow.com/questions/125281/how-do-i-remove-the-file-suffix-and-path-portion-from-a-path-string-in-bash
get_video_identifier(){
    echo ''
}

# List all multiple charpeters videos in current directory.
# ARGUEMNTS:
# OUTPUT:
#   A list of multi charpters video idetifier.
# TODO:
list_charptered_video() {
    for file in G*.MP4
    do
        local video_identifier=$(get_video_identifier $file)
        if [ $(is_charptered_video $video_identifier) -eq 1 ]; then
            echo $file_identifier
        fi
    done
}

# Archieve already merged charpter video files and other useless file.
clear_files(){
    if [ $? -eq 0 ]; then
        archieve_path="merged_chapter_videos/$1"
        # Archieve the already merged videos.
        mkdir -p $archieve_path
        mv G*${1}.{MP4,THM,LRV} $file_list_file $archieve_path
        echo "chapter videos are moved to $archieve_path."
    else
        echo "ffmpeg command exit with failed status."
    fi
}


# If run script without parameter, list video identifier of multiple charpters video.
if [ -z ${1+x} ]; then
    list_charptered_video $1
else
    # TODO input multiple files number and concat them all.
    file_list_file=.${1}.tmp
    
    echo "$(get_file_list $1)" > $file_list_file
    
    echo ffmpeg_file_list: $(cat $file_list_file)
    
    echo begin runnning ffmpeg!
    
    # TODO: add date information in output file name.
    # ffmpeg -f  -probesize 200 -analyzeduration 200 concat -i $file_list_file -c copy $1.mkv
    ffmpeg -f concat -i $file_list_file -codec copy $1.mkv
    # ffplay -f concat -i $file_list_file   $1.mkv
    
    if [ $? -eq 0 ]; then
        clear_files $1
    fi
fi
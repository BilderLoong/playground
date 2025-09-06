import { promises as fs } from 'fs';
import path from 'path';
import { exec as execCallback } from 'child_process';
import { promisify } from 'util';

const exec = promisify(execCallback);

// Define a type for a file processing result
interface FileResult {
    originalPath: string;
    outputPath: string;
    success: boolean;
    error?: string;
}

/**
 * Extracts audio from a video file to a temporary WAV file.
 * @param videoPath The full path to the video file.
 * @param tempDir The directory to save the temporary audio file.
 * @returns The path to the temporary WAV file.
 */
async function extractAudio(videoPath: string, tempDir: string): Promise<string> {
    const baseName = path.basename(videoPath, path.extname(videoPath));
    const tempAudioPath = path.join(tempDir, `${baseName}_temp.wav`);
    
    console.log(`Extracting audio from: ${videoPath}`);
    const command = `ffmpeg -i "${videoPath}" -vn -acodec pcm_s16le -y "${tempAudioPath}"`;
    await exec(command);
    
    return tempAudioPath;
}

/**
 * Cuts silence from an audio file using auto-editor.
 * @param inputAudioPath The path to the audio file to be edited.
 * @param outputAudioPath The desired path for the final output.
 */
async function cutSilence(inputAudioPath: string, outputAudioPath: string): Promise<void> {
    console.log(`Cutting silence from: ${inputAudioPath}`);
    // auto-editor creates a new file with the name specified by the output-file flag
    const command = `auto-editor "${inputAudioPath}" --output-file "${outputAudioPath}"`;
    await exec(command);
}

/**
 * Main function to process the video files.
 */
async function processVideos(): Promise<void> {
    // Get command line arguments
    const args = process.argv.slice(2);
    if (args.length < 2) {
        console.error('Usage: ts-node processVideos.ts <output_directory> <video_file1> <video_file2> ...');
        process.exit(1);
    }
    
    // Validate output directory
    const outputDir = path.resolve(args[0]);
    try {
        await fs.mkdir(outputDir, { recursive: true });
    } catch (error) {
        console.error(`Error: Could not create output directory at ${outputDir}`);
        process.exit(1);
    }
    
    // Get list of video files
    const videoFiles = args.slice(1).map(file => path.resolve(file));
    
    console.log(`Starting video processing for ${videoFiles.length} file(s)...`);
    
    for (const videoFile of videoFiles) {
        let tempAudioFile: string | undefined;
        let cutAudioFile: string | undefined;

        try {
            const baseName = path.basename(videoFile, path.extname(videoFile));
            const finalOutputPath = path.join(outputDir, `${baseName}_silence_cutted.m4a`);

            // Step 1: Extract audio from video to a temporary WAV file
            tempAudioFile = await extractAudio(videoFile, outputDir);

            // Step 2: Cut silence from the temporary WAV file
            // auto-editor output will be saved in a WAV format
            cutAudioFile = path.join(outputDir, `${baseName}_silence_cutted.wav`);
            await cutSilence(tempAudioFile, cutAudioFile);

            // Step 3: Convert the silence-cutted WAV file to M4A (AAC)
            console.log(`Converting to M4A: ${finalOutputPath}`);
            const convertCommand = `ffmpeg -i "${cutAudioFile}" -acodec aac -b:a 192k -y "${finalOutputPath}"`;
            await exec(convertCommand);

            console.log(`Successfully processed: ${videoFile} -> ${finalOutputPath}\n`);
        } catch (error) {
            console.error(`Error processing ${videoFile}:`, error);
        } finally {
            // Step 4: Clean up temporary files
            try {
                if (tempAudioFile) await fs.unlink(tempAudioFile);
                if (cutAudioFile) await fs.unlink(cutAudioFile);
            } catch (err) {
                console.error('Error cleaning up temporary files:', err);
            }
        }
    }
    
    console.log('Video processing complete.');
}

// Run the main function
processVideos();
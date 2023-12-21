import React, { useEffect } from "react";
import { useFilePicker } from "use-file-picker";

function stopEvent(e: React.UIEvent) {
  e.stopPropagation();
  e.preventDefault();
}

// TODO: Test on file drop.
export default function FileUploader({
  onFileSelect,
}: {
  onFileSelect: (fileList: FileList | File[]) => void;
}) {
  const { openFilePicker, loading, plainFiles, clear, errors } = useFilePicker({
    readFilesContent: false,
  });

  const handleInputOnChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;
    if (files) {
      onFileSelect(files);
    }
  };

  useEffect(() => {
    if (plainFiles.length && plainFiles[0] instanceof Blob) {
      onFileSelect(plainFiles);
    }
  }, [plainFiles, onFileSelect]);

  const handleDropFile = async (e: React.DragEvent<HTMLElement>) => {
    stopEvent(e);

    const files = e.dataTransfer.files;
    if (files) {
      onFileSelect(files);
    }

    // It seems that the stream version parser doesn't Web API.
    // So choose the sync parser.
    // TODO move this to subtitle viewer.
    // const node = parseSync(await file.text())
    // node.map(e => {
    //     if (e.type === 'header') {
    //         return e.data
    //     }

    //     return e.data.text
    // })
  };

  if (errors.length) {
    return <div>{JSON.stringify(errors)}</div>;
  }

  if (loading) {
    return <div>Loading</div>;
  }

  return (
    <section
      onDrop={handleDropFile}
      onDragOver={stopEvent}
      onDragEnter={stopEvent}
      onClick={openFilePicker}
      className="bg-slate-500"
      data-testid="file-uploader"
      aria-label="File uploader"
    >
      Click or drop here to choose files.
      {/* <input
        onChange={handleInputOnChange}
        type="file"
        id="input"
        accept=".srt,.vtt,.mp4,.avi,.mov,.mp3,.wav,.mkv,.flv,.wmv,.m4a,.m4v,.3gp,.aac,.ac3,.flac,.ogg,.opus,.webm"
        multiple
      /> */}
    </section>
  );
}

package util

import "compress/gzip"
import "encoding/binary"
import "errors"
import "fmt"
import "io"
import "io/ioutil"
import "os"

import "google.golang.org/protobuf/proto"

type FileZipReader struct {
  file *os.File
  zip_reader *gzip.Reader
}

type FileZipWriter struct {
  file *os.File
  zip_writer *gzip.Writer
}

func NewFileZipReader(filepath string) (*FileZipReader, error) {
  var err error
  var reader FileZipReader
  reader.file, err = os.Open(filepath)
  if err != nil { return nil, err }
  reader.zip_reader, err = gzip.NewReader(reader.file)
  if err != nil { return nil, err }
  return &reader, nil
}

func NewFileZipWriter(filepath string) (*FileZipWriter, error) {
  var err error
  var writer FileZipWriter
  writer.file, err = os.Create(filepath)
  if err != nil { return nil, err }
  writer.zip_writer = gzip.NewWriter(writer.file)
  return &writer, nil
}

func (self *FileZipReader) Close() error {
  var err error
  if self.zip_reader != nil { err = self.zip_reader.Close() }
  if err != nil && self.file != nil { err = self.file.Close() }
  return err
}

func (self *FileZipWriter) Close() error {
  var err error
  if self.zip_writer != nil { err = self.zip_writer.Close() }
  if err != nil && self.file != nil { err = self.file.Close() }
  return err
}

func (self *FileZipReader) Read(buf []byte) (int, error) {
  if self.zip_reader != nil {
    return self.zip_reader.Read(buf)
  }
  return 0, fmt.Errorf("zip_reader is nil")
}

func (self *FileZipWriter) Write(buf []byte) (int, error) {
  if self.zip_writer != nil {
    return self.zip_writer.Write(buf)
  }
  return 0, fmt.Errorf("zip_writer is nil")
}

// https://developers.google.com/protocol-buffers/docs/techniques#streaming
func WriteProtoWithPrefixedLen(writer io.Writer, msg proto.Message) (int, error) {
  var err error
  var buf []byte
  var written int
  buf, err = proto.Marshal(msg)
  if err != nil { return 0, err }
  err = binary.Write(writer, binary.LittleEndian, uint32(len(buf)))
  if err != nil { return 0, err }
  written, err = writer.Write(buf)
  return written+4, err
}

// May return EOF on last successful read
func ReadProtoWithPrefixedLen(reader io.Reader, msg proto.Message) (int, error) {
  var err error
  var pb_size uint32
  var read int

  err = binary.Read(reader, binary.LittleEndian, &pb_size)
  if err != nil {
    if !errors.Is(err, io.EOF) { return 0, err }
    proto.Reset(msg)
    // We do not know if we read 0 or we were already at EOF before reading pb_size
    if pb_size == 0 { return 0, err }
    return 0, fmt.Errorf("Expected %d more bytes in file", pb_size)
  }

  buf := make([]byte, pb_size)
  read, err = reader.Read(buf)
  if err != nil {
    if !errors.Is(err, io.EOF) { return read+4, err }
  }

  err = proto.Unmarshal(buf, msg)
  return read+4, err
}

func ReadProtoFromZipFile(filepath string, msg proto.Message) error {
  var buf []byte
  reader, err := NewFileZipReader(filepath)
  if err != nil { return err }
  defer reader.Close()

  buf, err = ioutil.ReadAll(reader)
  if err != nil { return err }
  err = proto.Unmarshal(buf, msg)
  return err
}

func WriteProtoIntoZipFile(filepath string, msg proto.Message) error {
  var buf []byte
  writer, err := NewFileZipWriter(filepath)
  if err != nil { return err }
  defer writer.Close()
  buf, err = proto.Marshal(msg)
  _, err = writer.Write(buf)
  return err
}


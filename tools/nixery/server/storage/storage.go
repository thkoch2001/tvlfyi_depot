// Package storage implements an interface that can be implemented by
// storage backends, such as Google Cloud Storage or the local
// filesystem.
package storage

import (
	"io"
	"net/http"
)

type Backend interface {
	// Name returns the name of the storage backend, for use in
	// log messages and such.
	Name() string

	// Persist provides a user-supplied function with a writer
	// that stores data in the storage backend.
	//
	// It needs to return the SHA256 hash of the data written as
	// well as the total number of bytes, as those are required
	// for the image manifest.
	Persist(string, func(io.Writer) (string, int64, error)) (string, int64, error)

	// Fetch retrieves data from the storage backend.
	Fetch(path string) (io.ReadCloser, error)

	// Move renames a path inside the storage backend. This is
	// used for staging uploads while calculating their hashes.
	Move(old, new string) error

	// Serve provides a handler function to serve HTTP requests
	// for layers in the storage backend.
	ServeLayer(digest string, w http.ResponseWriter) error
}

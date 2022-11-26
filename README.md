# slow-lamb

The goal for this project is to create a utility sitting between an audio source and output. It should be able to process a stream of audio in realtime. It will implement (niavely) a few audio proccesing algorithms such as time stretch, delay, repitch, and reverb.

Haskell was chosen as it allows a stream of incoming audio bytes to be treated as an infinite array. This way, the algorithms can be written to lazily process an array without the need for any non-IO, stream specific logic.

TODO: 
  - [x] File IO
  - [x] Command line parser
  - [x] Time stretch algorithm
  - [x] Delay algorithm
  - [ ] Reverb algorithm
  - [ ] Re-pitch algorithm
  - [ ] Stream IO

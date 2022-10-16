#!/bin/bash
mkdir mocked_test_files
cd mocked_test_files

# Multiple chapters videos.
touch GX{01..03}00{01..10}.{MP4,THM,LRV}

# Single chapter videos.
touch GX{04..06}0001.{MP4,THM,LRV}
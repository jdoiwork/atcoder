#!/bin/bash

# 引数が2つまたは3つ必要
if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
  echo "Usage: $0 <arg1> <arg2> [<prefix>]"
  exit 1
fi

# 引数3が指定されている場合はそれを使用、指定されていない場合は"abc"を使用
PREFIX="${3:-abc}"

# 問題用のディレクトリを作成する
# ./[prefix][引数1]/[引数2]
TARGET_DIR="./${PREFIX}$1/$2"

# TARGET_DIRが存在する場合は終了する
if [ -d "$TARGET_DIR" ]; then
  echo "Directory $TARGET_DIR already exists. Exiting."
  exit 1
fi

mkdir -p "$TARGET_DIR"

# templatesディレクトリの中身をコピーする
cp -r ./templates/* "$TARGET_DIR"

# 作成したディレクトリの中身を表示する
tree "$TARGET_DIR"

# Main.hsを開く
code "$TARGET_DIR/Main.hs"

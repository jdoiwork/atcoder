#!/bin/bash

# 引数が2つ必要
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <arg1> <arg2>"
  exit 1
fi

# 問題用のディレクトリを作成する
# ./abc[引数1]/[引数2]

TARGET_DIR="./abc$1/$2"

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

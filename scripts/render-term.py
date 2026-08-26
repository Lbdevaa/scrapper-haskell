#!/usr/bin/env python3
"""Рисует SVG-картинку терминала из текстового файла.

Использование: python scripts/render-term.py вход.txt выход.svg "заголовок окна"

Строки, начинающиеся с "$", считаются введёнными командами и подсвечиваются.
"""
import io
import sys
from xml.sax.saxutils import escape

FONT_SIZE = 14
CHAR_W = 8.42
LINE_H = 20
PAD_X = 16
PAD_TOP = 44
PAD_BOTTOM = 16

BG = "#11111b"
CHROME = "#1e1e2e"
TEXT = "#cdd6f4"
PROMPT = "#a6e3a1"
DIM = "#7f849c"
TITLE = "#9399b2"
DOTS = ("#f38ba8", "#f9e2af", "#a6e3a1")


def render(lines, title):
    width = max([len(line) for line in lines] + [len(title) + 8])
    width = int(width * CHAR_W) + PAD_X * 2
    height = PAD_TOP + len(lines) * LINE_H + PAD_BOTTOM

    out = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}" font-family="ui-monospace, SFMono-Regular, '
        f'Menlo, Consolas, monospace" font-size="{FONT_SIZE}">',
        f'<rect width="{width}" height="{height}" rx="10" fill="{BG}"/>',
        f'<path d="M0 10a10 10 0 0 1 10-10h{width - 20}a10 10 0 0 1 10 10v22H0z" fill="{CHROME}"/>',
    ]
    for i, color in enumerate(DOTS):
        out.append(f'<circle cx="{18 + i * 18}" cy="16" r="6" fill="{color}"/>')
    out.append(
        f'<text x="{width / 2}" y="21" fill="{TITLE}" font-size="12" '
        f'text-anchor="middle">{escape(title)}</text>'
    )

    for row, line in enumerate(lines):
        y = PAD_TOP + row * LINE_H
        if not line.strip():
            continue
        if line.startswith("$"):
            out.append(
                f'<text x="{PAD_X}" y="{y}" fill="{PROMPT}">{escape("$")}</text>'
                f'<text x="{PAD_X + CHAR_W}" y="{y}" fill="{TEXT}" '
                f'xml:space="preserve">{escape(line[1:])}</text>'
            )
        else:
            fill = DIM if line.startswith("---") else TEXT
            out.append(
                f'<text x="{PAD_X}" y="{y}" fill="{fill}" '
                f'xml:space="preserve">{escape(line)}</text>'
            )

    out.append("</svg>")
    return "\n".join(out)


def main():
    src, dst, title = sys.argv[1], sys.argv[2], sys.argv[3]
    lines = io.open(src, encoding="utf-8").read().replace("\t", "    ").rstrip("\n").split("\n")
    io.open(dst, "w", encoding="utf-8", newline="\n").write(render(lines, title))
    print(f"{dst}: {len(lines)} строк")


if __name__ == "__main__":
    main()

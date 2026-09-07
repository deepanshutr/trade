#!/usr/bin/env python3
"""A local child for exercising the installed interaction driver's controls."""
import os
import select
import sys
import termios
import time
import tty

mode = os.environ['DESK_CHECK_CASE']
if mode == 'child_failure':
    sys.exit(7)
fd = sys.stdin.fileno()
saved = termios.tcgetattr(fd)
tty.setraw(fd)
esc = chr(27)
coin = 'XMR'
prompt = None
target = False
selections = 0
wide = False
candles = False
zoom = 1
pending = b''
last_byte = 0.0
paste = False

def frame():
    global wide
    cols, rows = os.get_terminal_size(fd)
    wide = wide or cols == 120
    if cols == 120:
        if mode in ('resize_stuck', 'wrong_width'):
            cols = 100
        if mode in ('resize_stuck', 'wrong_height'):
            rows = 30
    health = 'active 0.1s'
    if wide and selections >= 3 and not target and cols == 100:
        if mode == 'stale':
            health = 'stale 5.5s'
        elif mode == 'disconnected':
            health = 'disconnected 0.1s'
    if cols < 100 or rows < 30:
        lines = [f'need 100x30, have {cols}x{rows}']
    else:
        lines = [f'connection {health}  {coin}  mid 100']
        lines += [''] * (rows - 6)
        lines += [' c candles 1m/candle ' if candles else f' c line {zoom}s/col ']
        if target:
            lines += [f'Target {coin} buy limit $100 | Enter disabled Escape cancels',
                      'request 1 estimate 1 value $100 average $100 | up/down price',
                      'position unavailable | 1-8 and left/right inactive', 'View only']
        else:
            lines += ['Hard 1 buy $500 2 buy $1000 3 sell $500 4 sell $1000',
                      'Best 5 buy $100 6 buy $100 7 sell $100 8 sell $100',
                      'Target 9 buy 0 sell | up/down zoom left/right history m market c chart q quit',
                      'View only']
        lines = [line.ljust(cols) for line in lines]
    end = '' if mode == 'missing_end' else esc + '[J'
    sys.stdout.write(esc + '[H' + (esc + '[K\r\n').join(lines) + esc + '[K' + end)
    sys.stdout.flush()

try:
    frame()
    running = True
    while running:
        if select.select([fd], [], [], 0.05)[0]:
            incoming = os.read(fd, 1024)
            if not incoming:
                break
            pending += incoming
            last_byte = time.monotonic()
        while pending:
            if paste:
                end = pending.find(b'\x1b[201~')
                if end < 0:
                    break
                pending = pending[end + 6:]
                paste = False
                continue
            if pending.startswith(b'\x1b[200~'):
                pending = pending[6:]
                paste = True
                continue
            if pending.startswith(b'\x1b['):
                if len(pending) < 3:
                    break
                arrow, pending = pending[2], pending[3:]
                if not target and not candles:
                    if arrow == ord('B'):
                        zoom = 2
                    elif arrow == ord('A'):
                        zoom = 1
                continue
            byte = pending[0]
            if byte == 27 and len(pending) == 1 and time.monotonic() - last_byte < 0.05:
                break
            pending = pending[1:]
            if byte == 27:
                target = False
                prompt = None
            elif prompt is not None:
                if byte == 13:
                    coin, prompt = prompt, None
                else:
                    prompt += chr(byte)
            elif byte == ord('m'):
                prompt = ''
                target = False
            elif byte == ord('c'):
                candles = not candles
            elif byte == ord('9'):
                target = True
                selections += 1
            elif byte == ord('q'):
                running = False
        if running:
            frame()
finally:
    sys.stdout.write(esc + '[?2004l' + esc + '[?25h' + esc + '[?1049l')
    sys.stdout.flush()
    if mode != 'restoration_failure':
        termios.tcsetattr(fd, termios.TCSANOW, saved)

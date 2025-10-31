#!/usr/bin/env python3
"""
plot_pltchi_multi.py

Usage:
  python3 plot_pltchi_multi.py file1.csv [file2.csv ...] [--out fig.png]

Behavior:
  - Plots each CSV as a line (auto colors).
  - Reads sidecar metadata from <file>.meta for labels/axes/title (if present).
  - If >=2 CSVs are given, computes residual = y2 - y1 over the overlap of x-ranges
    (x from file1, y2 interpolated onto file1’s x within the overlap) and plots it
    on a secondary y-axis (right side).
"""
import argparse, csv, os, math
from typing import Dict, List, Tuple, Optional
import numpy as np
import matplotlib.pyplot as plt


plt.rcParams.update({
    "font.size": 18,        # controls default text sizes
    "axes.titlesize": 20,   # fontsize of the axes title
    "axes.labelsize": 20,   # fontsize of the x and y labels
    "xtick.labelsize": 16,  # fontsize of the tick labels
    "ytick.labelsize": 16,
    "legend.fontsize": 16,  # legend fontsize
})

def load_meta(meta_path: str) -> Dict[str, str]:
    meta = {}
    try:
        with open(meta_path, "r") as f:
            for line in f:
                line = line.rstrip("\n")
                if ":" in line:
                    k, v = line.split(":", 1)
                    meta[k.strip().lower()] = v.strip()
    except FileNotFoundError:
        pass
    return meta

def read_xy(csv_path: str) -> Tuple[np.ndarray, np.ndarray, Dict[str,str]]:
    # Find a sidecar .meta next to the CSV
    base, _ = os.path.splitext(csv_path)
    meta = load_meta(base + ".meta")

    xs: List[float] = []
    ys: List[float] = []
    with open(csv_path, newline="") as f:
        r = csv.reader(f)
        header = next(r, None)
        # If header is present and looks like 'x,y' or 'x1,y1,x2,y2', we’ll just ignore names and
        # read the first two numeric columns per row.
        for row in r:
            if not row:
                continue
            # pick first two numeric entries
            nums = []
            for cell in row:
                cell = cell.strip()
                try:
                    nums.append(float(cell))
                except ValueError:
                    continue
                if len(nums) == 2:
                    break
            if len(nums) == 2:
                xs.append(nums[0]); ys.append(nums[1])
    x = np.asarray(xs, dtype=float)
    y = np.asarray(ys, dtype=float)
    return x, y, meta

def label_from_meta(meta: Dict[str,str], fallback: str) -> str:
    # Prefer legend1/title from meta; otherwise filename
    for k in ("legend1", "title"):
        if k in meta and meta[k]:
            return meta[k]
    return fallback

def axis_labels_from_meta(metas: List[Dict[str,str]]) -> Tuple[str, str, Optional[str]]:
    # Try to get consensus labels; fall back gracefully
    xlabel = next((m.get("xlabel") for m in metas if "xlabel" in m), "x")
    ylabel = next((m.get("ylabel") for m in metas if "ylabel" in m), "y")
    title  = next((m.get("title")  for m in metas if "title"  in m), None)
    xlabel = xlabel.replace("cm-1",r"$\text{cm}^{-1}$")
    ylabel = ylabel.replace("SQRT(Intensity)",r"$\sqrt{I}$")
    ylabel = ylabel.replace("Theoretical Susceptibility", r"$|\chi^{(3)}|_{\mathrm{theory}}$")
    return xlabel, ylabel, title

def compute_residual(x1: np.ndarray, y1: np.ndarray, x2: np.ndarray, y2: np.ndarray
                    ) -> Tuple[np.ndarray, np.ndarray]:
    # Overlap is [max(mins), min(maxes)]
    xlo = max(float(np.min(x1)), float(np.min(x2)))
    xhi = min(float(np.max(x1)), float(np.max(x2)))
    if not (xhi > xlo):
        return np.array([]), np.array([])

    # mask x1 to overlap and interpolate y2 onto x1 grid
    mask = (x1 >= xlo) & (x1 <= xhi)
    x1o = x1[mask]
    if x1o.size < 2:
        return np.array([]), np.array([])

    # Ensure x2 is strictly increasing for interpolation
    order = np.argsort(x2)
    x2s, y2s = x2[order], y2[order]
    # Remove duplicate x’s to keep np.interp happy
    uniq, idx = np.unique(x2s, return_index=True)
    x2u, y2u = x2s[idx], y2s[idx]

    y2_on_x1 = np.interp(x1o, x2u, y2u)
    resid = y2_on_x1 - y1[mask]
    return x1o, resid

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("csvs", nargs="+", help="One or more CSV files (from PLTCHI replacement)")
    ap.add_argument("--out", help="Save figure to this path instead of showing it")
    args = ap.parse_args()

    series = []
    metas  = []
    for csv_path in args.csvs:
        x, y, meta = read_xy(csv_path)
        if x.size == 0:
            print(f"Warning: no numeric data in {csv_path}")
            continue
        label = label_from_meta(meta, os.path.basename(csv_path))
        series.append((x, y, label, csv_path))
        metas.append(meta)

    if not series:
        print("No valid data to plot.")
        return

    # Figure and axes
    fig, ax = plt.subplots()
    ax2 = None

    # Plot all provided datasets
    xmin = math.inf; xmax = -math.inf
    ymin = math.inf; ymax = -math.inf
    for (x, y, label, path) in series:
        ax.plot(x, y, label=label)
        if x.size:
            xmin = min(xmin, float(np.min(x)))
            xmax = max(xmax, float(np.max(x)))
        if y.size:
            ymin = min(ymin, float(np.min(y)))
            ymax = max(ymax, float(np.max(y)))

    # Residual for the FIRST TWO datasets, if present
    if len(series) >= 2:
        x1, y1, l1, _ = series[0]
        x2, y2, l2, _ = series[1]
        xr, rr = compute_residual(x1, y1, x2, y2)
        if xr.size:
            ax2 = ax.twinx()
            ax2.plot(xr, rr, linestyle="--", label=f"residual ({l2} - {l1})")
            # autoscale residual around 0 with a bit of padding
            rmin, rmax = float(np.min(rr)), float(np.max(rr))
            pad = 0.05 * max(abs(rmin), abs(rmax), 1e-12)
            ax2.set_ylim(rmin - pad, rmax + pad)
            ax2.set_ylabel("Residual")
        else:
            print("Note: first two series have no overlapping x-range; residual skipped.")

    # Axis labels/title from metadata (best-effort)
    xlabel, ylabel, title = axis_labels_from_meta(metas)
    print(f"XLABEL:({xlabel}), YLABEL:({ylabel})")
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    title = None
    if title:
        ax.set_title(title)

    # Set x/y limits based on combined data
    if xmin < xmax:
        ax.set_xlim(xmin, xmax)
    if ymin < ymax:
        # Expand a touch to avoid clipping
        py = 0.02 * max(abs(ymin), abs(ymax))
        ax.set_ylim(ymin - py, ymax + py)

    # Build a combined legend (primary + secondary axes if present)
    handles, labels = ax.get_legend_handles_labels()
    if ax2:
        h2, l2 = ax2.get_legend_handles_labels()
        handles += h2; labels += l2
    # if handles:
    #    ax.legend(handles, labels, loc="best")

    plt.tight_layout()
    if args.out:
        fig.savefig(args.out, dpi=150)
    else:
        plt.show()

if __name__ == "__main__":
    main()

import csv
import math
from collections import defaultdict, namedtuple
from decimal import Decimal, InvalidOperation
import sys

INPUT_CSV = "./creditcard_2023.csv"
TOP50_CSV = "./top50_groups.csv"
EPS = 1e-8   

GroupAgg = namedtuple("GroupAgg", ["bin", "start", "end", "count", "count_neg_v7", "mean_v11", "total_amount"])

def safe_decimal(s):
    s = s.strip()
    if s == "":
        raise InvalidOperation("empty")
    return Decimal(s)

def compute_aggregates(input_csv):
    groups = defaultdict(lambda: {"count":0, "count_neg_v7":0, "sum_v11":Decimal(0), "total_amount":Decimal(0)})
    with open(input_csv, newline='') as f:
        reader = csv.reader(f)
        try:
            headers = next(reader)
        except StopIteration:
            raise RuntimeError("Пустой файл: " + input_csv)
        hmap = {h.strip(): i for i,h in enumerate(headers)}
        for key in ("V1","V7","V11","Amount"):
            if key not in hmap:
                raise RuntimeError(f"В заголовке нет колонки '{key}'")
        ci_v1 = hmap["V1"]
        ci_v7 = hmap["V7"]
        ci_v11 = hmap["V11"]
        ci_amount = hmap["Amount"]

        line_no = 1
        for row in reader:
            line_no += 1
            if len(row) <= max(ci_v1,ci_v7,ci_v11,ci_amount):
                continue
            try:
                amount = safe_decimal(row[ci_amount])
            except Exception:
                continue
            if amount < Decimal("9000.00"):
                continue
            try:
                v1 = safe_decimal(row[ci_v1])
                v7 = safe_decimal(row[ci_v7])
                v11 = safe_decimal(row[ci_v11])
            except Exception:
                continue
            bin_index = int(math.floor(float(v1 / Decimal("0.1"))))
            g = groups[bin_index]
            g["count"] += 1
            if float(v7) < 0.0:
                g["count_neg_v7"] += 1
            g["sum_v11"] += v11
            g["total_amount"] += amount

    result = []
    for b, stats in groups.items():
        cnt = stats["count"]
        if cnt == 0:
            continue
        mean_v11 = stats["sum_v11"] / Decimal(cnt)
        start = Decimal(b) * Decimal("0.1")
        end = start + Decimal("0.1")
        result.append(GroupAgg(bin=b,
                               start=float(start),
                               end=float(end),
                               count=stats["count"],
                               count_neg_v7=stats["count_neg_v7"],
                               mean_v11=float(mean_v11),
                               total_amount=float(stats["total_amount"])))
    return result

def select_top50_from_aggregates(aggs):
    aggs_sorted = sorted(aggs, key=lambda g: ( -g.count_neg_v7, -g.mean_v11 ))
    candidates = []
    i = 0
    while i < len(aggs_sorted) and len(candidates) < 50:
        cur_neg = aggs_sorted[i].count_neg_v7
        j = i
        while j < len(aggs_sorted) and aggs_sorted[j].count_neg_v7 == cur_neg:
            candidates.append(aggs_sorted[j])
            j += 1
        i = j
    # from candidates pick top 50 by mean_v11 desc (tie-breaker total_amount desc)
    candidates_sorted = sorted(candidates, key=lambda g: (-g.mean_v11, -g.total_amount))
    top50 = candidates_sorted[:50]
    # final ordering by total_amount desc
    top50_sorted = sorted(top50, key=lambda g: -g.total_amount)
    return top50_sorted

def read_top50_csv(path):
    rows = []
    with open(path, newline='') as f:
        reader = csv.reader(f)
        try:
            header = next(reader)
        except StopIteration:
            raise RuntimeError("Пустой файл: " + path)
        # expected header: bin_start,bin_end,bin_index,count,count_neg_v7,mean_V11,total_amount
        for r in reader:
            if not r or len(r) < 7:
                continue
            try:
                start = float(r[0])
                end = float(r[1])
                idx = int(float(r[2]))
                count = int(float(r[3]))
                neg = int(float(r[4]))
                mean_v11 = float(r[5])
                tot = float(r[6])
            except Exception:
                continue
            rows.append(GroupAgg(bin=idx, start=start, end=end, count=count, count_neg_v7=neg, mean_v11=mean_v11, total_amount=tot))
    return rows

def compare_lists(calc, given):
    # compare by bin index sets and order
    calc_bins = [g.bin for g in calc]
    given_bins = [g.bin for g in given]
    set_calc = set(calc_bins)
    set_given = set(given_bins)
    missing = set_calc - set_given
    extra = set_given - set_calc
    # positional differences
    positional_mismatches = []
    for pos, (gb, cb) in enumerate(zip(given_bins, calc_bins)):
        if gb != cb:
            positional_mismatches.append( (pos+1, gb, cb) )
    # value differences for matched bins
    value_diffs = []
    calc_map = {g.bin:g for g in calc}
    for g in given:
        b = g.bin
        if b in calc_map:
            c = calc_map[b]
            # compare numeric fields with tolerance
            if abs(c.mean_v11 - g.mean_v11) > 1e-6 or abs(c.total_amount - g.total_amount) > 1e-4 or c.count != g.count or c.count_neg_v7 != g.count_neg_v7:
                value_diffs.append( (b, c, g) )
    return {
        "missing_bins": missing,
        "extra_bins": extra,
        "positional_mismatches": positional_mismatches,
        "value_diffs": value_diffs
    }

def pretty_print_group(g):
    return f"bin {g.bin} [{g.start},{g.end}) cnt={g.count} neg={g.count_neg_v7} meanV11={g.mean_v11:.10f} total={g.total_amount:.10f}"

def main():
    print("Вычисляю агрегаты из", INPUT_CSV)
    try:
        aggs = compute_aggregates(INPUT_CSV)
    except Exception as e:
        print("Ошибка при чтении/агрегации:", e)
        sys.exit(2)
    print("Всего бинов после фильтра:", len(aggs))

    print("Формирую top50 по вашей логике...")
    calc_top50 = select_top50_from_aggregates(aggs)
    print("Получено топ-50 бинов на основе исходного файла.")

    print("Читаю предоставленный файл", TOP50_CSV)
    try:
        given_top50 = read_top50_csv(TOP50_CSV)
    except Exception as e:
        print("Ошибка при чтении", TOP50_CSV, ":", e)
        sys.exit(3)
    print("В файле найдено строк:", len(given_top50))

    comp = compare_lists(calc_top50, given_top50)

    if not comp["missing_bins"] and not comp["extra_bins"] and not comp["positional_mismatches"] and not comp["value_diffs"]:
        print("\nОК: файл совпадает с вычисленным результатом — бины, значения и порядок идентичны.")
        return

    print("\nНайдены расхождения:")

    if comp["missing_bins"]:
        print("\nБины, которые есть в рассчитанном топ-50, но отсутствуют в вашем файле (missing):")
        for b in sorted(comp["missing_bins"]):
            g = next(x for x in calc_top50 if x.bin == b)
            print("  ", pretty_print_group(g))

    if comp["extra_bins"]:
        print("\nБины, которые есть в вашем файле, но отсутствуют в рассчитанном топ-50 (extra):")
        for b in sorted(comp["extra_bins"]):
            g = next(x for x in given_top50 if x.bin == b)
            print("  ", pretty_print_group(g))

    if comp["positional_mismatches"]:
        print("\nПозиционные несовпадения (позиция, ваш_bin, вычисленный_bin):")
        for pos, your_bin, calc_bin in comp["positional_mismatches"]:
            print(f"  pos {pos}: your={your_bin}, calc={calc_bin}")

    if comp["value_diffs"]:
        print("\nОтличия по значениям для совпадающих бинов (bin, вычисл. => ваш):")
        for b, c, g in comp["value_diffs"]:
            print(f"  bin {b}:")
            print(f"    calc: cnt={c.count}, neg={c.count_neg_v7}, meanV11={c.mean_v11:.10f}, total={c.total_amount:.10f}")
            print(f"    your: calc? cnt={g.count}, neg={g.count_neg_v7}, meanV11={g.mean_v11:.10f}, total={g.total_amount:.10f}")

    print("\nЗавершено. Если хотите, могу также вывести CSV всех агрегатов (весь список бинов) для ручной проверки.")

if __name__ == "__main__":
    main()

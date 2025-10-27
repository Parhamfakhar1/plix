# 🤔 Plix Language — Syntax Reference

> نسخه 1.0 — طراحی و توسعه توسط **Parham Fakhari**
> این سند راهنمای کامل سینتکس و ساختار زبان **Plix** است.

---

## 📑 فهرست مطالب

* [تعریف متغیرها](#-تعریف-متغیرها)
* [چاپ مقدار](#-چاپ-مقدار)
* [کنترل جریان](#-کنترل-جریان)

  * [شرط‌ها](#-شرطها)
  * [حلقه‌ها](#-حلقهها)
* [توابع](#-توابع)
* [مدیریت خطا](#-مدیریت-خطا)
* [تطبیق الگو](#-تطبیق-الگو)
* [شیءگرایی](#-شیءگرایی)
* [مقادیر خاص](#-مقادیر-خاص)
* [عملگرها](#-عملگرها)
* [ساختارهای داده](#-ساختارهای-داده)

  * [لیست‌ها](#-لیستها)
  * [دیکشنری‌ها](#-دیکشنریها)
  * [رشته‌های قالب‌دار](#-رشتههای-قالبدار)
* [نکات تکمیلی](#-نکات-تکمیلی)

---

## 🔹 تعریف متغیرها

برای تعریف متغیر از `var` استفاده می‌شود:

```plix
var name = "Parham"
var age = 16
```

---

## 🔹 چاپ مقدار

برای نمایش خروجی در کنسول از دستور `say` استفاده می‌شود:

```plix
say "Hello, World!"
say $"My name is {name} and I am {age} years old."
```

---

## 🔹 کنترل جریان

### ✅ شرط‌ها

```plix
if age >= 18 {
    say "Adult"
} eif age >= 13 {
    say "Teenager"
} else {
    say "Child"
}
```

---

### 🔁 حلقه‌ها

#### while

```plix
var i = 0
while i < 5 {
    say i
    i = i + 1
}
```

#### for ... in

```plix
for x in [1, 2, 3, 4] {
    say x
}
```

#### کنترل حلقه

```plix
for x in [1, 2, 3, 4, 5] {
    if x == 3 {
        continue
    }
    if x == 5 {
        break
    }
    say x
}
```

---

## 🔹 توابع

برای تعریف تابع از `func` و برای بازگرداندن مقدار از `return` استفاده می‌شود:

```plix
func greet(name) {
    say $"Hello {name}!"
}

func add(a, b) {
    return a + b
}
```

---

## 🔹 مدیریت خطا

مدیریت خطا با `try`, `catch` و `throw` انجام می‌شود:

```plix
try {
    throw "Something went wrong!"
} catch err {
    say $"Error: {err}"
}
```

---

## 🔹 تطبیق الگو (Pattern Matching)

```plix
match day {
    case "Saturday" {
        say "Weekend!"
    }
    case "Sunday" {
        say "Weekend!"
    }
    default {
        say "Workday"
    }
}
```

---

## 🔹 شیءگرایی

Plix از مفاهیم پایه شیءگرایی شامل کلاس، وراثت، و `this` پشتیبانی می‌کند.

```plix
class Animal {
    func init(name) {
        this.name = name
    }

    func speak() {
        say $"The animal {this.name} makes a sound."
    }
}

class Dog from Animal {
    func speak() {
        super.speak()
        say $"Dog {this.name} barks!"
    }
}

var d = Dog("Buddy")
d.speak()
```

---

## 🔹 مقادیر خاص

| مقدار   | توضیح                   |
| ------- | ----------------------- |
| `true`  | مقدار بولی درست         |
| `false` | مقدار بولی نادرست       |
| `nil`   | مقدار تهی یا بدون مقدار |

---

## 🔹 عملگرها

| نوع               | عملگر             | توضیح                  |
| ----------------- | ----------------- | ---------------------- |
| ریاضی             | `+ - * /`         | جمع، تفریق، ضرب، تقسیم |
| مقایسه            | `== != < > <= >=` | مقایسه مقادیر          |
| منطقی             | `and or !`        | عملگرهای منطقی         |
| تخصیص             | `=`               | مقداردهی به متغیر      |
| محدوده            | `..`              | تولید بازه عددی        |
| دسترسی به عضو     | `.`               | دسترسی به متد یا ویژگی |
| رشته‌های قالب‌دار | `$`               | قالب‌بندی رشته‌ها      |

---

## 🔹 ساختارهای داده

### 📋 لیست‌ها

```plix
var numbers = [1, 2, 3, 4]
say numbers[0]
```

**لیست کامپریهنشن (List Comprehension):**

```plix
var doubled = [x * 2 for x in numbers]
```

---

### 🗝️ دیکشنری‌ها

```plix
var user = {
    name: "Parham",
    age: 16,
    active: true
}

say user.name
```

---

### 🧩 رشته‌های قالب‌دار

```plix
var name = "Parham"
say $"Welcome {name}!"
```

---

## 🔹 نکات تکمیلی

* تمام متغیرها **دینامیک تایپ‌شده** هستند.
* بلوک‌های کد با `{ ... }` مشخص می‌شوند.
* تمام کلمات کلیدی **کوچک‌حرف (lowercase)** هستند.
* پشتیبانی از **inline expressions** در رشته‌های قالب‌دار وجود دارد.

---

> © 2025 Plix Language — Designed by Parham Fakhari

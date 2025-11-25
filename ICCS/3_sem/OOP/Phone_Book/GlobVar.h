#ifndef GLOBVAR_H
#define GLOBVAR_H

#define ColumnCount 7
namespace {
static const char* DateMask = "dd.MM.yyyy";
static const char* fslname_regex = "(^[A-Z])([A-Z|a-z|0-9| |-]*)([A-Z|a-z|0-9]$)";
static const char* email_regex = "(^[a-z|A-Z|0-9]+)@([a-z|A-Z|0-9]+\\.[a-z|A-Z|0-9]+$)";
static const char* phone_regex = "\\+\\d{1,3}\\(\\d{3}\\)\\d{3}\\-\\d{2}\\-\\d{2}";
}

#endif // GLOBVAR_H

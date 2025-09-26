CC = gcc
CFLAGS = -Wall -Wextra -O2 -std=c11
LDFLAGS = -lz

SRCDIR = src
OBJDIR = obj
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
TARGET = buildpy

.PHONY: all clean install test

all: $(TARGET)

$(TARGET): $(OBJECTS)
	@$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)
	@$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR):
	@mkdir -p $(OBJDIR)

clean:
	@rm -rf $(OBJDIR) $(TARGET)

install: $(TARGET)
	@cp $(TARGET) /usr/local/bin/

test: $(TARGET)
	@./$(TARGET) --help
	@./$(TARGET) --version 3.12.9 --config static_max --verbose

.SUFFIXES:
.SUFFIXES: .c .o
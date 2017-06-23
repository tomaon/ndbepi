#
ID ?= 15700

MYSQL_HOME ?= /opt/mysql/release/5.7.18-ndb-7.5.6
MYSQL_CONF ?= $(PWD)/etc/v57.cnf
MYSQL_BASE ?= /tmp/ndbepi

SOPT  = --defaults-file=$(MYSQL_CONF)
SOPT += --datadir=$(MYSQL_BASE)/data
SOPT += --log-error=$(MYSQL_BASE)/mysqld.log
SOPT += --secure-file-priv=$(PWD)
SOPT += --server-id=$(ID)
SOPT += --socket=/tmp/mysqld.$(ID).sock

COPT  = --defaults-file=$(MYSQL_CONF)
COPT += --socket=/tmp/mysqld.$(ID).sock
COPT += --user $(1)

#
default: wireshark

#
$(VERBOSE).SILENT:

$(MYSQL_BASE):
	mkdir -p $@ && $(MYSQL_HOME)/bin/mysqld $(SOPT) --initialize-insecure
start: $(MYSQL_BASE)
	$(MYSQL_HOME)/bin/mysqld $(SOPT) &

ping processlist reload status variables version:
	$(MYSQL_HOME)/bin/mysqladmin $(call COPT, root) $@
stop:
	$(MYSQL_HOME)/bin/mysqladmin $(call COPT, root) shutdown

run-root:
	$(MYSQL_HOME)/bin/mysql $(call COPT, root) --prompt="id:$(ID)> "
run-%:
	$(MYSQL_HOME)/bin/mysql $(call COPT, $*) -p $* --prompt="id:$(ID)> "

clean:
	rm -rf $(MYSQL_BASE)

#
wireshark:
	sudo wireshark -X lua_script:ndbapi.lua

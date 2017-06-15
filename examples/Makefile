MYSQL_HOME = /opt/mysql/debug/5.7.17-ndb-7.5.5
MYSQL_CONF = $(PWD)/etc/v57.cnf
MYSQL_BASE = /tmp/ndbepi

SOPT  = --defaults-file=$(MYSQL_CONF)
SOPT += --datadir=$(MYSQL_BASE)/data
SOPT += --log-error=$(MYSQL_BASE)/mysqld.log
SOPT += --socket=/tmp/mysqld.sock

COPT  = --defaults-file=$(MYSQL_CONF)
COPT += --socket=/tmp/mysqld.sock
COPT += --user $(1)

#
default: wireshark

#
$(VERBOSE).SILENT:

$(MYSQL_BASE):
	mkdir -p $@ && $(MYSQL_HOME)/bin/mysqld $(SOPT) --initialize-insecure
start: $(MYSQL_BASE)
	$(MYSQL_HOME)/bin/mysqld $(SOPT) --ndbcluster &

ping processlist reload status variables version:
	$(MYSQL_HOME)/bin/mysqladmin $(call COPT, root) $@
stop:
	$(MYSQL_HOME)/bin/mysqladmin $(call COPT, root) shutdown

run-root:
	$(MYSQL_HOME)/bin/mysql $(call COPT, root)

clean: rm-data

#
rm-data:
	rm -rf $(MYSQL_BASE)

#
wireshark:
	wireshark -X lua_script:ndbapi.lua

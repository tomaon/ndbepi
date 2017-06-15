ID ?= 15700

MYSQL_HOME ?= /opt/mysql/debug/5.7.17-ndb-7.5.5
MYSQL_CONF ?= $(PWD)/etc/v57.cnf
MYSQL_DATA ?= /tmp/ndbepi

SOPT  = --defaults-file=$(MYSQL_CONF)
SOPT += --datadir=$(MYSQL_DATA)/$(ID)/data
SOPT += --log-error=$(MYSQL_DATA)/$(ID)/mysqld.log
SOPT += --server-id=$(ID)
SOPT += --socket=/tmp/mysqld.$(ID).sock

COPT  = --defaults-file=$(MYSQL_CONF)
COPT += --socket=/tmp/mysqld.$(ID).sock
COPT += --user $(1)

#
default: wireshark

#
$(VERBOSE).SILENT:

$(MYSQL_DATA): | $(MYSQL_DATA)-$(ID)
	$(MYSQL_HOME)/bin/mysqld $(SOPT) --initialize-insecure
start: $(MYSQL_DATA)
	$(MYSQL_HOME)/bin/mysqld $(SOPT) --ndbcluster &

stop:
	$(MYSQL_HOME)/bin/mysqladmin $(call COPT, root) shutdown

run-root:
	$(MYSQL_HOME)/bin/mysql $(call COPT, root)

clean: rm-data

#
$(MYSQL_DATA)-%:
	mkdir -p $(MYSQL_DATA)/$*

rm-data:
	rm -rf $(MYSQL_DATA)

#
wireshark:
	wireshark -X lua_script:ndbapi.lua

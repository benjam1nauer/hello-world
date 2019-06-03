ifndef BUILD_TYPE
  $(error Set environment variable BUILD_TYPE (Debug|Release) )
endif

# Ordner des Projektes
pwd= $(shell cygpath -d `pwd`)
pwd:=$(subst \,/,$(pwd))

CMAKE_ARGS= -G"Unix Makefiles"
CMAKE_ARGS+= -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)
CMAKE_ARGS+= -DCMAKE_INSTALL_PREFIX=$(WZL_INSTALL_BASE)-$(BUILD_TYPE)

all: $(BUILD_TYPE)/Makefile
	cd $(BUILD_TYPE) && time $(MAKE)

$(BUILD_TYPE)/Makefile: $(BUILD_TYPE)
	cd $(BUILD_TYPE) && cmake $(CMAKE_ARGS) ../
	
$(BUILD_TYPE):
	mkdir $(BUILD_TYPE)
		
clean:
	cd $(BUILD_TYPE) && $(MAKE)	clean
		
purge:
	rm -rf $(BUILD_TYPE)/*[!log]	
	
rebuild:
	$(MAKE) purge 
	$(MAKE) 			
		
install: all
	cd $(BUILD_TYPE) && $(MAKE)	install

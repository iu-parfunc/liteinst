#!/usr/bin/python

import os;

pwd = os.getcwd()

libs = ['libtests_reg.so', 'libtests_deact.so', 'libtests_act.so']
bins = ['test_app_0_O0.exe', 'test_app_1_O3.exe']

for test in bins:
  for lib in libs:
    cmd = "LD_PRELOAD=%s/%s ./apps/%s" % (pwd, lib, test)
    os.system(cmd)

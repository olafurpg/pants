# Copyright 2017 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

python_tests(
  sources = ['test_distribution.py'],
  dependencies = [
    '3rdparty/python/twitter/commons:twitter.common.collections',
    'src/python/pants/base:revision',
    'src/python/pants/java/distribution',
    'src/python/pants/util:contextutil',
    'src/python/pants/util:dirutil',
    'tests/python/pants_test/subsystem:subsystem_utils',
  ],
  timeout = 15,
)

python_tests(
  name = 'distribution_integration',
  sources = ['test_distribution_integration.py'],
  dependencies = [
    '3rdparty/python/twitter/commons:twitter.common.collections',
    'src/python/pants/java/distribution',
    'src/python/pants/util:osutil',
    'tests/python/pants_test:int-test',
    'tests/python/pants_test/subsystem:subsystem_utils',
  ],
  tags = {'integration'},
  timeout = 180,
)

from setuptools import setup

setup(
    name='main',
    version='0.0.1',
    py_modules=['main'],
    entry_points={
      'console_scripts': ['main = main:run']
    },
)

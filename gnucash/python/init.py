import sys
from runpy import run_path
from pathlib import Path

from gnucash._sw_core_utils import gnc_build_userdata_path


user_init = Path(gnc_build_userdata_path("python")) / "init.py"
default_init = Path(__file__).parent / "default_init.py"

sys.path.extend(str(p.parent) for p in [user_init, default_init])

run_path(str(user_init if user_init.exists() else default_init),
         run_name="__main__")

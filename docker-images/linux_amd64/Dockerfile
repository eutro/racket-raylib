FROM ubuntu:16.04
RUN apt-get update -y
RUN apt-get install ca-certificates tzdata -y
RUN apt-get install git build-essential cmake -y
RUN apt-get install libasound2-dev mesa-common-dev libx11-dev libxrandr-dev libxi-dev xorg-dev libgl1-mesa-dev libglu1-mesa-dev -y
RUN useradd builder --shell /bin/bash --create-home
ENTRYPOINT ["/bin/bash"]

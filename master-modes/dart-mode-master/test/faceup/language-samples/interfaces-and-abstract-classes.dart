class MockSpaceship implements Spacecraft {
  // ยทยทยท
}

abstract class Describable {
  void describe();

  void describeWithEmphasis() {
    print('=========');
    describe();
    print('=========');
  }
}

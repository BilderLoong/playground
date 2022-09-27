describe('mock1', () => {
  it('should return right mocked value', () => {
    const mock1 = jest.fn();
    expect(mock1()).toBe(undefined);

    mock1
      .mockReturnValueOnce(10)
      .mockReturnValueOnce('123')
      .mockReturnValueOnce(true);

    expect(mock1()).toBe(10);
  });
});
